#include <HsFFI.h>
#include <lua.h>
#include <lauxlib.h>
#include "hslauxlib.h"
#include "hslcall.h"

/* Auxiliary Library */

/*
** Creates a new Lua state and set extra registry values for error
** bookkeeping.
*/
lua_State *hsluaL_newstate()
{
  lua_State *L = luaL_newstate();

  /* add error value */
  lua_createtable(L, 0, 0);
  lua_setfield(L, LUA_REGISTRYINDEX, HSLUA_ERR);

  /* register HaskellFunction userdata metatable */
  hslua_registerhsfunmetatable(L);

  return L;
}


/*
** Helper for hsluaL_tostring
*/
static int hsluaL__tolstring(lua_State *L)
{
  luaL_tolstring(L, 1, NULL);
  return 1;
}

/*
** Converts object to string, respecting any metamethods; returns NULL
** if an error occurs.
*/
const char *hsluaL_tolstring(lua_State *L, int index, size_t *len)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hsluaL__tolstring);
  lua_insert(L, -2);
  int res = lua_pcall(L, 1, 1, 0);
  if (res != LUA_OK) {
    /* error */
    return NULL;
  }
  return lua_tolstring(L, -1, len);
}
