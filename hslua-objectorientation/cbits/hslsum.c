#include <lua.h>
#include <lauxlib.h>
#include <string.h>
#include "hslobj.h"
#include "hslsum.h"

/* ***************************************************************
 * Abstract Data Type access
 * ***************************************************************/

int hslua_gettag(lua_State *L, int idx)
{
  return lua_getiuservalue(L, idx, 2);
}

int hslua_sum_get_tag(lua_State *L)
{
  if (hslua_gettag(L, 1) == LUA_TSTRING) {
    return 1;
  }

  lua_pop(L, 1);
  return 0;
}

static int hsluaS_get_constructor_table(lua_State *L, int obj)
{
  if (!lua_getmetatable(L, obj))        /* no metatable? */
    return LUA_TNIL;
  else {
    hslua_sum_get_tag(L);
    int tt = lua_rawget(L, -2);
    if (tt != LUA_TTABLE) {
      lua_pop(L, 2);                    /* remove metatable and metafield */
      return LUA_TNIL;
    } else {
      lua_remove(L, -2);                /* remove metatable */
      return tt;                        /* return metafield type */
    }
  }
}

int hslua_sum_field(lua_State *L, int idx, const char *field)
{
  int tt = hsluaS_get_constructor_table(L, idx);
  if (tt != LUA_TTABLE) {               /* No constructor table? */
    return tt;
  }

  tt = lua_getfield(L, -1, field);
  lua_remove(L, -2);                    /* remove constructor table */
  return tt;                            /* return constructor field type */
}

int hslua_sum_peekers(lua_State *L, int idx)
{
  return hslua_sum_field(L, idx, "peekers");
}

static int hsluaS_get_constructor_field(lua_State *L)
{
  luaL_checkstack(L, 5, "hsluaS_get_constructor_field");

  if (hsluaS_get_constructor_table(L, 1) == LUA_TNIL) {
    return 0; /* fail if object has no string tag */
  }

  /* get getter function */
  lua_getfield(L, -1, "getters");
  lua_pushvalue(L, 2);
  if (lua_rawget(L, -2) != LUA_TFUNCTION) {
    return 0;
  }

  /* Call getter. Slow, as it calls into Haskell. */
  lua_pushvalue(L, 1);
  lua_call(L, 1, 1);

  /* FIXME: this was copy-pasted, dry it up! */
  /* key found in wrapped userdata, add to caching table */
  hslua_get_caching_table(L, 1);        /* object's caching table */
  lua_pushvalue(L, 2);                  /* key */
  lua_pushvalue(L, -3);                 /* value */
  lua_rawset(L, -3);
  lua_pop(L, 1);                        /* pop caching table */
  /* return value */
  return 1;
}

/*
** FIXME
*/
int hslua_sum_udindex(lua_State *L)
{
  lua_settop(L, 2);
  return
    hsluaO_get_from_cache(L) ||
    hsluaS_get_constructor_field(L) ||
    hslua_udindex(L);
}

int hsluaS_set_via_setter(lua_State *L)
{
  if (hsluaS_get_constructor_table(L, 1) == LUA_TNIL) {
    return 0;
  }

  /* get setter function */
  lua_getfield(L, -1, "setters");
  lua_pushvalue(L, 2);
  if (lua_rawget(L, -2) != LUA_TFUNCTION) {
    return 0;
  }

  /* call setter */
  lua_insert(L, 1);
  lua_pop(L, 2);                       /* pop constructor & setter tables */
  lua_call(L, 3, 0);

  return 1;
}

/*
** The actual assignment is performed by a setter function stored in the
** `setter` metafield. Throws an error if no setter function can be
** found.
*/
int hslua_sum_udnewindex(lua_State *L)
{
  if (hsluaS_set_via_setter(L) || hslua_udnewindex(L)) {
    return 0;
  }

  return luaL_error(L, "Cannot set property on sum-type object");
}
