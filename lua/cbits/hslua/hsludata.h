#include <HsFFI.h>
#include <lua.h>
#include <lauxlib.h>

/* ***************************************************************
 * Userdata for Haskell values
 * ***************************************************************/

/*
** Creates a new userdata metatable for Haskell objects.
*/
int hslua_newudmetatable(lua_State *L, const char *tname);
