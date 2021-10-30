#include <string.h>

#include "lua.h"

int luaopen_lpeg (lua_State *L);
int luaopen_lpeg (lua_State *L)
{
  const char *msg =
  lua_pushliteral(L, "Non-functional placeholder. Configuration prevented "
                  "the inclusion of the real LPeg package.");
  lua_error(L);
}
