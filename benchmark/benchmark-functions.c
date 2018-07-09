#include <string.h>
#include "benchmark-functions.h"

/*
** getlfield
*/
int hslua__getlfield(lua_State *L)
{
  lua_gettable(L, 1);
  return 1;
}

int hslua_getlfield(lua_State *L, int index, const char *k, size_t len)
{
  lua_pushvalue(L, index);
  lua_pushlstring(L, k, len);
  lua_pushcfunction(L, hslua__getlfield);
  lua_insert(L, -3);
  return -lua_pcall(L, 2, 1, 0);
}

/*
** setfield
*/
int hslua__setfield(lua_State *L)
{
  const char *k = lua_tostring(L, 3);
  lua_pushvalue(L, 1);
  lua_setfield(L, 2, k);
  return 0;
}

int hslua_setfield(lua_State *L, int index, const char *k)
{
  lua_pushvalue(L, index);
  lua_pushstring(L, k);
  lua_pushcfunction(L, hslua__setfield);
  lua_insert(L, -4);
  return -lua_pcall(L, 3, 0, 0);
}
