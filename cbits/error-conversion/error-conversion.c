#include <stdio.h>
#include <string.h>
#include <HsFFI.h>
#include "error-conversion.h"

/* *********************************************************************
 * Transforming Haskell errors to Lua errors
 * *********************************************************************/
void hslua_pushhaskellerr(lua_State *L)
{
  lua_getfield(L, LUA_REGISTRYINDEX, "HSLUA_ERR");
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


/* *********************************************************************
 * Transforming Lua errors to Haskell errors
 * *********************************************************************/

/*
** compare
*/
int hslua__compare(lua_State *L)
{
  int op = lua_tointeger(L, 3);
  int res = lua_compare(L, 1, 2, op);
  lua_pushinteger(L, res);
  return 1;
}

int hslua_compare(lua_State *L, int index1, int index2, int op, int *status)
{
  index1 = lua_absindex(L, index1);
  index2 = lua_absindex(L, index2);
  lua_pushcfunction(L, hslua__compare);
  lua_pushvalue(L, index1);
  lua_pushvalue(L, index2);
  lua_pushinteger(L, op);
  *status = lua_pcall(L, 3, 1, 0);
  if (*status != LUA_OK) {
    return 0;
  }
  int result = lua_tointeger(L, -1);
  lua_pop(L, 1);
  return result;
}


/*
** concat
*/
int hslua__concat(lua_State *L)
{
  lua_concat(L, lua_gettop(L));
  return 1;
}

void hslua_concat(lua_State *L, int n, int *status)
{
  lua_pushcfunction(L, hslua__concat);
  lua_insert(L, -n - 1);
  *status = lua_pcall(L, n, 1, 0);
}


/*
** getglobal
*/
int hslua__getglobal(lua_State *L)
{
  lua_gettable(L, 1);
  return 1;
}

void hslua_getglobal(lua_State *L, const char *name, size_t len, int *status)
{
  lua_pushcfunction(L, hslua__getglobal);
  lua_pushglobaltable(L);
  lua_pushlstring(L, name, len);
  *status = lua_pcall(L, 2, 1, 0);
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

void hslua_gettable(lua_State *L, int index, int *status)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__gettable);
  lua_insert(L, -3);
  *status = lua_pcall(L, 2, 1, 0);
}


/*
** setglobal
*/
int hslua__setglobal(lua_State *L)
{
  /* index 1: value */
  /* index 2: the global table */
  /* index 3: key */
  lua_pushvalue(L, 1);
  lua_settable(L, 2);
  return 0;
}

void hslua_setglobal(lua_State *L, const char *name, size_t len, int *status)
{
  /* we expect the new value to be at the top of the stack */
  lua_pushglobaltable(L);
  lua_pushlstring(L, name, len);
  lua_pushcfunction(L, hslua__setglobal);
  lua_insert(L, -4);
  *status = lua_pcall(L, 3, 0, 0);
}


/*
** settable
*/
int hslua__settable(lua_State *L)
{
  lua_pushvalue(L, 1); /* key */
  lua_pushvalue(L, 2); /* value */
  lua_settable(L, 3);  /* table is the third argument */
  return 0;
}

void hslua_settable(lua_State *L, int index, int *status)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__settable);
  lua_insert(L, -4);
  *status = lua_pcall(L, 3, 0, 0);
}


/*
** next
*/
int hslua__next(lua_State *L)
{
  lua_pushvalue(L, 1);
  return lua_next(L, 2) ? 2 : 0;
}

int hslua_next(lua_State *L, int index, int *status)
{
  int oldsize = lua_gettop(L);
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__next);
  lua_insert(L, -3);
  *status = lua_pcall(L, 2, LUA_MULTRET, 0);
  if (*status != 0) {
    /* error */
    return 0;
  }
  /* success */
  return (lua_gettop(L) - oldsize + 1); /* correct for popped value */
}


/*
** Auxiliary Library
*/

/*
** tolstring'
*/
int hsluaL__tolstring(lua_State *L)
{
  luaL_tolstring(L, 1, NULL);
  return 1;
}

const char *hsluaL_tolstring(lua_State *L, int index, size_t *len)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hsluaL__tolstring);
  lua_insert(L, -2);
  int res = lua_pcall(L, 1, 1, 0);
  if (res != 0) {
    /* error */
    return NULL;
  }
  return lua_tolstring(L, -1, len);
}
