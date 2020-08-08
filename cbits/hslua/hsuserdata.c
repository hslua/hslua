#include <HsFFI.h>
#include "hslua.h"

/* ***************************************************************
 * Garbage Collection
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

int hslua_newudmetatable(lua_State *L, const char *tname)
{
  if (luaL_newmetatable(L, tname)) {
    lua_pushboolean(L, 1);
    lua_setfield(L, -2, "__metatable");
    lua_pushcfunction(L, &hslua_userdata_gc);
    lua_setfield(L, -2, "__gc");
    return 1;  /* new table created */
  }
  return 0;    /* table not created */
}

/*
** Creates a new userdata containing a Haskell value (or rather, a
** stable pointer to a Haskell value) and pushes it to the stack.
**
** Returns `0` if the accompanying metatable already existed, and
** `1` if it was newly created.
*/
int hslua_newuserdata(lua_State *L,
                      HsStablePtr data,
                      const char *tname)
{
  HsStablePtr *ud = lua_newuserdata(L, sizeof data);
  *ud = data;
  int mt_created = hslua_newudmetatable(L, tname);
  lua_pushvalue(L, -1);    /* push another ref to mt onto stack */
  lua_setmetatable(L, -3); /* set metatable for UD */

  return mt_created;       /* whether metatable was created */
}
