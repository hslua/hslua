/* ***************************************************************
 * Functions exported from Haskell
 * ***************************************************************/

#include <HsFFI.h>
#include <lua.h>

/*  exported from Foreign.Lua.Raw.Call */
int hslua_callhsfun(lua_State *L);
