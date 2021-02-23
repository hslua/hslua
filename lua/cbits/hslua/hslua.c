#include <HsFFI.h>
#include <stdio.h>
#include <string.h>
#include "hslua.h"
#include "hslcall.h"

/* ***************************************************************
 * Transforming Lua errors to Haskell errors
 * ***************************************************************/

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

/*
** Compares two values on the stack. Behaves mostly like lua_compare,
** but takes an additional parameter `status`, the referent of which is
** assigned the result of calling the helper function.
 */
int hslua_compare(lua_State *L, int index1, int index2, int op, int *status)
{
  index1 = lua_absindex(L, index1);
  index2 = lua_absindex(L, index2);
  lua_pushcfunction(L, hslua__compare);
  lua_pushvalue(L, index1);
  lua_pushvalue(L, index2);
  lua_pushinteger(L, op);
  int outcome = lua_pcall(L, 3, 1, 0);
  if (status != NULL) {
    *status = outcome;
  }
  if (outcome != LUA_OK) {
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

int hslua_getglobal(lua_State *L, const char *name, size_t len, int *status)
{
  lua_pushcfunction(L, hslua__getglobal);
  lua_pushglobaltable(L);
  lua_pushlstring(L, name, len);
  *status = lua_pcall(L, 2, 1, 0);
  return lua_type(L, -1);
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

int hslua_gettable(lua_State *L, int index, int *status)
{
  lua_pushvalue(L, index);
  lua_pushcfunction(L, hslua__gettable);
  lua_insert(L, -3);
  *status = lua_pcall(L, 2, 1, 0);
  return lua_type(L, -1);
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
