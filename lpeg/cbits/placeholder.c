#include <string.h>
#include "lua.h"

int luaopen_lpeg (lua_State *L);
int luaopen_lpeg (lua_State *L)
{
  lua_pushliteral(L, "Non-functional placeholder. Configuration prevented "
                  "the inclusion of the real LPeg package.");
  lua_error(L);
}

int luaopen_re (lua_State *L);
int luaopen_re (lua_State *L)
{
  lua_pushliteral(L, "Non-functional placeholder. Configuration prevented "
                     "the inclusion of the real 're' package.");
  lua_error(L);
}
