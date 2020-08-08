#include <HsFFI.h>
#include <lua.h>

/* ***************************************************************
 * Userdata for Haskell values
 * ***************************************************************/

/*
** Free stable Haskell pointer in userdata.
**
** The userdata whose contents is garbage collected must be on
** stack index 1 (i.e., the first argument).
*/
int hslua_userdata_gc(lua_State *L);
