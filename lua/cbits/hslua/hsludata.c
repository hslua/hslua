#include <HsFFI.h>
#include "hslua.h"
#include "hsludata.h"

/* ***************************************************************
 * Userdata for Haskell values
 * ***************************************************************/

/*
** Free stable Haskell pointer in userdata.
**
** The userdata whose contents is garbage collected must be on
** stack index 1 (i.e., the first argument).
*/
int hslua_userdata_gc(lua_State *L)
{
  HsStablePtr *userdata = lua_touserdata(L, 1);
  if (userdata) {
    hs_free_stable_ptr(*userdata);
  }
  return 0;
}

/*
** Creates a new userdata metatable for Haskell objects, or gets
** is from the registry if possible.
**
** Returns `true` if the metatable was created, and `false` if it
** already existed and was fetched from the registry.
*/
int hslua_newudmetatable(lua_State *L, const char *tname)
{
  int created = luaL_newmetatable(L, tname);
  if (created) {
    /* Prevent accessing or changing the metatable with
     * getmetatable/setmetatable. */
    lua_pushboolean(L, 1);
    lua_setfield(L, -2, "__metatable");
    /* Mark objects for finalization when collecting garbage. */
    lua_pushcfunction(L, &hslua_userdata_gc);
    lua_setfield(L, -2, "__gc");
  }
  return created;
}
