#include <lua.h>
#include <lauxlib.h>
#include "hslcall.h"

/* *********************************************************************
 * Transforming Haskell errors to Lua errors
 * *********************************************************************/
void hslua_pushhaskellerr(lua_State *L)
{
  lua_getfield(L, LUA_REGISTRYINDEX, HSLUA_ERR);
}

/*
** Marks the occurence of an error; the returned value should be used as
** the error message.
*/
int hslua_error(lua_State *L)
{
  hslua_pushhaskellerr(L);
  lua_insert(L, -2);
  return 2;
}

/*
** Checks whether the object at the given index is a Haskell error.
*/
int hslua_is_haskell_error(lua_State *L, int idx) {
  hslua_pushhaskellerr(L);
  int is_err = lua_rawequal(L, idx, -1);
  lua_pop(L, 1);                /* pop haskellerr used for equality test */
  return is_err;
}

/*
** Converts a Haskell function into a CFunction.
**
** We signal an error on the haskell side by passing two values: the
** special haskellerr object and the error message. The function
** returned an error iff there are exactly two results objects where the
** first object is the special HSLUA_ERR registry entry.
*/
int hslua_call_hs(lua_State *L)
{
  int nargs = lua_gettop(L);
  /* Push HaskellImportFunction and call the underlying function */
  lua_pushvalue(L, lua_upvalueindex(1));
  lua_insert(L, 1);
  lua_call(L, nargs, LUA_MULTRET);

  /* Check whether an error value was returned */
  int nres = lua_gettop(L);

  /* If there are two results, the first of which is the special error
   * object, then the other object is thrown as an error.
   */
  if (nres == 2 && hslua_is_haskell_error(L, 1)) {
    return lua_error(L);        /* throw 2nd return value as error */
  }

  return nres;
}
