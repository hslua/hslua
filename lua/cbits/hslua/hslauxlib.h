#ifndef hslauxlib_h
#define hslauxlib_h

#include "lua.h"
#include "lauxlib.h"

/* Auxiliary Library */

/*
** Creates a new state for use with the Haskell bindings.
*/
lua_State *hsluaL_newstate();

/*
** Converts object to string, respecting any metamethods; returns NULL
** if an error occurs.
*/
const char *hsluaL_tolstring(lua_State *L, int index, size_t *len);

#endif
