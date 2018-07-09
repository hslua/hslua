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
