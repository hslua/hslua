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

static int auxrequiref(lua_State *L)
{
  const char *modname = lua_tolstring(L, 1, NULL);
  lua_CFunction openf = lua_tocfunction(L, 2);
  int glb = lua_toboolean(L, 3);
  luaL_requiref(L, modname, openf, glb);
  return 1;
}

/*
** Simple version of `require` used to load modules from a C function.
*/
void hsluaL_requiref (lua_State *L, const char *modname,
                      lua_CFunction openf, int glb, int *status)
{
  lua_pushcfunction(L, &auxrequiref);
  lua_pushstring(L, modname);
  lua_pushcfunction(L, openf);
  lua_pushboolean(L, glb);
  int pstatus = lua_pcall(L, 3, 1, 0);
  if (status != NULL)
    *status = pstatus;
}
