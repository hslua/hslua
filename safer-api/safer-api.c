#include <stdio.h>
#include <string.h>
#include <HsFFI.h>
#include "safer-api.h"


/* *********************************************************************
 * Transforming Haskell errors to Lua errors
 * *********************************************************************/
void hslua_pushhaskellerr(lua_State *L)
{
  lua_getfield(L, LUA_REGISTRYINDEX, "HSLUA_ERR");
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

/* *********************************************************************
 * Garbage Collection
 * *********************************************************************/

/*
** Free stable Haskell pointer in userdata.
*/
int hslua_userdata_gc(lua_State *L)
{
  HsStablePtr *userdata = lua_touserdata(L, 1);
  if (userdata) {
    hs_free_stable_ptr(*userdata);
  }
  return 0;
}


/* *********************************************************************
 * Transforming Lua errors to Haskell errors
 * *********************************************************************/

/*
** compare
*/
#if LUA_VERSION_NUM >= 502
int hslua__compare(lua_State *L)
{
  int op = lua_tointeger(L, 3);
  int res = lua_compare(L, 1, 2, op);
  lua_pushinteger(L, res);
  return 1;
}

int hslua_compare(lua_State *L, int index1, int index2, int op)
{
  index1 = lua_absindex(L, index1);
  index2 = lua_absindex(L, index2);
  lua_pushcfunction(L, hslua__compare);
  lua_pushvalue(L, index1);
  lua_pushvalue(L, index2);
  lua_pushinteger(L, op);
  int callres = lua_pcall(L, 3, 1, 0);
  if (callres != 0) {
    return -callres;
  }
  int res = lua_tointeger(L, -1);
  lua_pop(L, 1);
  return res;
}
#endif


/*
** concat
*/
int hslua__concat(lua_State *L)
{
  lua_concat(L, lua_gettop(L));
  return 1;
}

int hslua_concat(lua_State *L, int n)
{
  lua_pushcfunction(L, hslua__concat);
  lua_insert(L, -n - 1);
  return -lua_pcall(L, n, 1, 0);
}


/*
** getfield
*/
int hslua__getfield(lua_State *L)
{
  const char *k = lua_tostring(L, 2);
  lua_getfield(L, 1, k);
  return 1;
}

int hslua_getfield(lua_State *L, int index, const char *k)
{
  lua_pushvalue(L, index);
  lua_pushlstring(L, k, strlen(k));
  lua_pushcfunction(L, hslua__getfield);
  lua_insert(L, -3);
  return -lua_pcall(L, 2, 1, 0);
}


/*
** getglobal
*/
int hslua__getglobal(lua_State *L)
{
  const char *name = lua_tostring(L, 1);
#if LUA_VERSION_NUM >= 502
  lua_getglobal(L, name);
#else
  lua_getfield(L, LUA_GLOBALSINDEX, name);
#endif
  return 1;
}

int hslua_getglobal(lua_State *L, const char *name)
{
  lua_pushcfunction(L, hslua__getglobal);
  lua_pushlstring(L, name, strlen(name));
  return -lua_pcall(L, 1, 1, 0);
}


/*
** gettable
*/
int hslua__gettable(lua_State *L)
{
  lua_pushvalue(L, 1);
  lua_gettable(L, 2);
  return 1;
}

int hslua_gettable(lua_State *L, int index)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__gettable);
  lua_insert(L, -3);
  return -lua_pcall(L, 2, 1, 0);
}


/*
** setfield
*/
int hslua__setfield(lua_State *L)
{
  const char *k = lua_tostring(L, 3);
  lua_pushvalue(L, 1);
  lua_setfield(L, 2, k);
  return 0;
}

int hslua_setfield(lua_State *L, int index, const char *k)
{
  lua_pushvalue(L, index);
  lua_pushlstring(L, k, strlen(k));
  lua_pushcfunction(L, hslua__setfield);
  lua_insert(L, -4);
  return -lua_pcall(L, 3, 0, 0);
}


/*
** setglobal
*/
int hslua__setglobal(lua_State *L)
{
  const char *name = lua_tostring(L, 2);
  lua_pushvalue(L, 1);
#if LUA_VERSION_NUM >= 502
  lua_setglobal(L, name);
#else
  lua_setfield(L, LUA_GLOBALSINDEX, name);
#endif
  return 0;
}

int hslua_setglobal(lua_State *L, const char *name)
{
  lua_pushlstring(L, name, strlen(name));
  lua_pushcfunction(L, hslua__setglobal);
  lua_insert(L, -3);
  return -lua_pcall(L, 2, 0, 0);
}


/*
** settable
*/
int hslua__settable(lua_State *L)
{
  lua_pushvalue(L, 1);
  lua_pushvalue(L, 2);
  lua_settable(L, 3);
  return 0;
}

int hslua_settable(lua_State *L, int index)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__settable);
  lua_insert(L, -4);
  return -lua_pcall(L, 3, 0, 0);
}


/*
** next
*/
int hslua__next(lua_State *L)
{
  lua_pushvalue(L, 1);
  return lua_next(L, 2) ? 2 : 0;
}

int hslua_next(lua_State *L, int index)
{
  int oldsize = lua_gettop(L);
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__next);
  lua_insert(L, -3);
  int res = lua_pcall(L, 2, LUA_MULTRET, 0);
  if (res != 0) {
    /* error */
    return (- res);
  }
  /* success */
  return (lua_gettop(L) - oldsize + 1); /* correct for popped value */
}
