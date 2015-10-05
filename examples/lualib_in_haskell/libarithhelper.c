#include "LibArith_stub.h"
#include "lua.h"

int hs_init_lua(lua_State *L)
{
  hs_init(NULL, NULL);
  return 0;
}

int hs_exit_lua(lua_State *L)
{
  hs_exit();
  return 0;
}

int luaopen_lualibhelper(lua_State *L)
{
  lua_pushcfunction(L, add);
  lua_setglobal(L, "add_in_haskell");
  lua_pushcfunction(L, hs_init_lua);
  lua_setglobal(L, "hs_init");
  lua_pushcfunction(L, hs_exit_lua);
  lua_setglobal(L, "hs_exit");
  return 0;
}
