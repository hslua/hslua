#include <lua.h>
#include <lauxlib.h>
#include "hslobj.h"

/* ***************************************************************
 * Lazy List object access
 * ***************************************************************/

/*
** Retrieve a numerical index from this object. The userdata must be in
** position 1, and the key in position 2.
*/
static int hsluaL_get_numerical(lua_State *L)
{
  hslua_get_caching_table(L, 1);
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
** Otherwise, the default method for key retrieval is used.
*/
int hslua_list_udindex(lua_State *L)
{
  lua_settop(L, 2);
  /* do numeric lookup for integer keys */
  return lua_isinteger(L, 2)
    ? (hsluaL_get_numerical(L))
    /* try various sources in order; return 0 if nothing is found. */
    : hslua_udindex(L);
}

/*
** Sets a numerical index on this object. The userdata must be in
** position 1, the key in position 2, and the new value in position 3.
** Returns 1 on success and 0 otherwise.
*/
static int hsluaL_set_numerical(lua_State *L)
{
  hslua_get_caching_table(L, 1);
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
      lua_pushcfunction(L, &hsluaL_get_numerical);
      lua_pushvalue(L, 1);              /* userdata object */
      lua_pushvalue(L, 2);              /* numerical key */
      lua_call(L, 2, 0);
    }
  }
  lua_pushvalue(L, 3);                  /* new value */
  lua_rawseti(L, -2, target);           /* set in caching table */
  return 1;  /* signal success */
}

/*
** Sets a value for a list-like object. Behaves like normal element
** access, but also handles (numerical) list indices.
*/
int hslua_list_udnewindex(lua_State *L)
{
  lua_settop(L, 3);
  if (lua_type(L, 2) == LUA_TNUMBER) {
    if (hsluaL_set_numerical(L)) {
      return 0;
    }
    lua_pushliteral(L, "Cannot set a numerical value.");
    return lua_error(L);
  }

  return hslua_udnewindex(L);
}
