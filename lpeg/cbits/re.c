#include "lua.h"
#include "lauxlib.h"
#include "re.h"

int luaopen_re(lua_State *L);
int luaopen_re(lua_State *L) {
  luaL_loadstring(L, cbits_lpeg_1_0_2_re_lua);
  lua_call(L, 0, 1);
  return 1;
}
