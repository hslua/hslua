#include <HsFFI.h>
#include <lua.h>
#include <lauxlib.h>
#include <string.h>

/* ***************************************************************
 * Helpers for fast element access
 * ***************************************************************/

/*
** Pushes the caching table of the userdata at index `idx` to the stack.
**
** Creates and sets a new table if none has been attached to the
** userdata yet.
*/
static void hsluaO_get_caching_table(lua_State *L, int idx)
{
  if (lua_getuservalue(L, idx) == LUA_TTABLE) {
    return;
  }

  /* No caching table set yet; create table and add to object. */
  lua_pop(L, 1);                        /* remove nil */

  int absidx = lua_absindex(L, idx);
  lua_createtable(L, 0, 0);
  lua_pushvalue(L, -1);
  lua_setuservalue(L, absidx);
}

/*
** Retrieve a value from the wrapped userdata project. The userdata must
** be in position 1, and the key in position 2. Returns 1 if a value was
** found and is at the top of the stack, 0 otherwise. Does not clean-up
** on success.
*/
static int hsluaO_get_from_cache(lua_State *L)
{
  /* Use value in caching table if present */
  hsluaO_get_caching_table(L, 1);       /* table */
  lua_pushvalue(L, 2);                  /* key */
  if (lua_rawget(L, 3) == LUA_TNIL) {
    lua_pop(L, 2);                      /* remove nil, caching table */
    return 0;
  }
  /* found the key in the cache */
  return 1;
}


/*
** Retrieve a value from the wrapped userdata project.
** The userdata must be in position 1, and the key in position 2.
 */
static int hsluaO_get_via_getter(lua_State *L)
{
  /* Bail if there are no getterns, or no getter for the given key. */
  if (luaL_getmetafield(L, 1, "getters") != LUA_TTABLE) {
    return 0;
  }
  lua_pushvalue(L, 2);                  /* key */
  if (lua_rawget(L, -2) == LUA_TNIL) {
    lua_pop(L, 1);
    return 0;
  }

  /* Call getter. Slow, as it calls into Haskell. */
  lua_pushvalue(L, 1);
  lua_call(L, 1, 1);

  /* key found in wrapped userdata, add to caching table */
  hsluaO_get_caching_table(L, 1);       /* object's caching table */
  lua_pushvalue(L, 2);                  /* key */
  lua_pushvalue(L, -3);                 /* value */
  lua_rawset(L, -3);
  lua_pop(L, 1);                        /* pop caching table */
  /* return value */
  return 1;
}

/*
** Retrieve a value by using the key as the alias for a different
** property. The userdata must be in position 1, and the key in position
** 2.
*/
static int hsluaO_get_via_alias(lua_State *L)
{
  if (luaL_getmetafield(L, 1, "aliases") != LUA_TTABLE) {
    return 0;             /* no aliases available */
  }
  lua_pushvalue(L, 2);
  if (lua_rawget(L, -2) != LUA_TTABLE) {
    lua_pop(L, 2);       /* key is not an alias */
    return 0;            /* try a different method */
  }

  /* key is an alias */
  lua_pushvalue(L, 1);    /* start with the original object */
  /* Iterate over properties; last object is on top of stack,
   * list of properties is the second object. */
  lua_Integer len = (lua_Integer) lua_rawlen(L, -2);
  for (lua_Integer i = 1; i <= len; i++) {
    lua_rawgeti(L, -2, i);
    int objtype = lua_gettable(L, -2);  /* get property */
    lua_remove(L, -2);    /* remove previous object */
    if (!objtype) break;  /* abort if this property of the alias is absent */
  }
  return 1;
}

/*
** Retrieve a method for this object. The userdata must be in position
** 1, and the key in position 2.
*/
static int hsluaO_get_method(lua_State *L)
{
  if (luaL_getmetafield(L, 1, "methods") != LUA_TTABLE) {
    lua_pop(L, 1);
    return 0;
  }
  lua_pushvalue(L, 2);
  lua_rawget(L, -2);
  return 1;
}

/*
** Retrieve a numerical index from this object. The userdata must be in
** position 1, and the key in position 2.
*/
static int hsluaO_get_numerical(lua_State *L)
{
  hsluaO_get_caching_table(L, 1);
  lua_Integer requested = lua_tointeger(L, 2);

  /* The __lazylistindex is set to `nil` or an integer if part of the
     list is still unevaluated. If it's `false`, then all list values are
     already in the cache. */
  if (lua_getfield(L, 1, "__lazylistindex") == LUA_TBOOLEAN) {
    lua_pop(L, 1);                      /* remove nil */
  } else {
    lua_Integer last_index = lua_tointeger(L, -1);
    lua_pop(L, 1);                      /* pop last-index value */

    if (requested > last_index &&
        /* index not in cache, force lazy evaluation of list items */
        luaL_getmetafield(L, 1, "lazylisteval") == LUA_TFUNCTION) {
      if (lua_getfield(L, 3, "__lazylist") != LUA_TUSERDATA) {
        /* lazy list thunk is missing; that shouldn't happen!!  */
        luaL_error(L, "Error while getting numerical index %d: "
                   "lazy list thunk is missing", requested);
      }
      lua_pushinteger(L, last_index);
      lua_pushinteger(L, requested);
      lua_pushvalue(L, 3);              /* caching table */
      lua_call(L, 4, 0);                /* populate cache with evaled values */
    }
  }
  lua_rawgeti(L, 3, requested);
  return 1;
}

/*
** Retrieves a key from a Haskell-data holding userdata value.
**
** If the key is an integer, any associated list is evaluated and the
** result is stored in the cache before it is returned.
**
** For non-integer keys, it tries the following, in order, and returns
** the first non-nil result:
**
**   + Checks the userdata's uservalue table for the given key;
**   + looks up a `getter` for the key and calls it with the userdata and
**     key as arguments;
**   + tries to lookup the key as an alias and retrieves the value of the
**     alias;
**   + looks up the key in the table in the `methods` metafield.
*/
int hslua_udindex(lua_State *L)
{
  lua_settop(L, 2);
  /* do numeric lookup for integer keys */
  return lua_isinteger(L, 2)
    ? (hsluaO_get_via_alias(L) || hsluaO_get_numerical(L))
    /* try various sources in order; return 0 if nothing is found. */
    : (hsluaO_get_from_cache(L) ||
       hsluaO_get_via_getter(L) ||
       hsluaO_get_via_alias(L)  ||
       hsluaO_get_method(L));
}

/*
** Set value via a property alias. Assumes the stack to be in a state as
** after __newindex is called. Returns 1 on success, and 0 otherwise.
 */
static int hsluaO_set_via_alias(lua_State *L)
{
  if (luaL_getmetafield(L, 1, "aliases") != LUA_TTABLE) {
    return 0;
  }
  lua_pushvalue(L, 2);
  if (lua_rawget(L, -2) != LUA_TTABLE) { /* key is an alias */
    lua_pop(L, 2);
    return 0;
  }
  lua_pushvalue(L, 1);    /* start with the original object */
  /* Iterate over properties; last object is on top of stack,
   * list of properties is the second object. */
  lua_Integer len = (lua_Integer) lua_rawlen(L, -2);
  for (int i = 1; i < len; i++) {
    lua_rawgeti(L, -2, i);
    lua_gettable(L, -2);  /* get property */
    lua_remove(L, -2);    /* remove previous object */
  }
  lua_rawgeti(L, -2, len); /* last element */
  lua_pushvalue(L, 3);     /* new value */
  lua_settable(L, -3);
  return 1;
}

/*
** Sets a numerical index on this object. The userdata must be in
** position 1, the key in position 2, and the new value in position 3.
** Returns 1 on success and 0 otherwise.
*/
static int hsluaO_set_numerical(lua_State *L)
{
  hsluaO_get_caching_table(L, 1);
  lua_Integer target = lua_tointeger(L, 2);

  /* The `__lazylistindex` field is set to `false` if each list element
     has already been evaluated and stored in the cache. Otherwise it
     will be either `nil` or an integer. */
  if (lua_getfield(L, 1, "__lazylistindex") == LUA_TBOOLEAN) {
    lua_pop(L, 1);                      /* pop boolean from last-index */
  } else {
    /* list is not fully evaluated yet, we may have to evaluate it
       further. */
    lua_Integer last_index = lua_tointeger(L, -1);
    lua_pop(L, 1);                      /* pop last-index value */

    if (target > last_index) {
      /* the index we want to assign has not been cached yet. Evaluation
       * is forced to avoid any uncertainty about the meaning of
       * `nil`-valued indices. */
      lua_pushcfunction(L, &hsluaO_get_numerical);
      lua_pushvalue(L, 1);
      lua_pushvalue(L, 2);
      lua_call(L, 2, 0);
    }
  }
  lua_pushvalue(L, 3);                  /* new value */
  lua_rawseti(L, -2, target);           /* set in caching table */
  return 1;
}

/*
** Set value via a property alias. Assumes the stack to be in a state as
** after __newindex is called. Returns 1 on success, 0 if the object is
** readonly, and throws an error if there is no setter for the given
** key.
*/
static int hsluaO_set_via_setter(lua_State *L)
{
  if (luaL_getmetafield(L, 1, "setters") != LUA_TTABLE)
    return 0;

  lua_pushvalue(L, 2);                  /* key */
  if (lua_rawget(L, -2) != LUA_TFUNCTION) {
    lua_pop(L, 1);
    lua_pushliteral(L, "Cannot set unknown property.");
    return lua_error(L);
  }

  lua_insert(L, 1);
  lua_settop(L, 4);       /* 1: setter, 2: ud, 3: key, 4: value */
  lua_call(L, 3, 0);
  return 1;
}

/*
** Sets a new value in the userdata caching table via a setter
** functions.
**
** The actual assignment is performed by a setter function stored in the
** `setter` metafield. Throws an error if no setter function can be
** found.
 */
int hslua_udnewindex(lua_State *L)
{
  if (lua_type(L, 2) == LUA_TNUMBER) {
    if (hsluaO_set_via_alias(L) || hsluaO_set_numerical(L)) {
      return 0;
    }
    lua_pushliteral(L, "Cannot set a numerical value.");
    return lua_error(L);
  }
  if (hsluaO_set_via_alias(L) || hsluaO_set_via_setter(L)) {
    return 0;
  }

  lua_pushliteral(L, "Cannot modify read-only object.");
  return lua_error(L);
}

/*
** Sets a value in the userdata's caching table (uservalue). Takes the
** same arguments as a `__newindex` function.
*/
int hslua_udsetter(lua_State *L)
{
  luaL_checkany(L, 3);
  lua_settop(L, 3);
  hsluaO_get_caching_table(L, 1);
  lua_insert(L, 2);
  lua_rawset(L, 2);
  return 0;
}

/*
** Throws an error noting that the given key is read-only.
*/
int hslua_udreadonly(lua_State *L)
{
  if (lua_type(L, 2) == LUA_TSTRING && lua_checkstack(L, 3)) {
    lua_pushliteral(L, "'");
    lua_pushvalue(L, 2);
    lua_pushliteral(L, "' is a read-only property.");
    lua_concat(L, 3);
  } else {
    lua_pushliteral(L, "Cannot set read-only value.");
  }
  return lua_error(L);
}
