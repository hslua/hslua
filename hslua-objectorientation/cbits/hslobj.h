#ifndef hslobj_h
#define hslobj_h

#include <lua.h>

/* ***************************************************************
 * Helpers for fast element access
 * ***************************************************************/

/* Object field getter */
int hslua_udindex(lua_State *L);

/* Object field setter */
int hslua_udnewindex(lua_State *L);

/* Lazy access to object's caching table */
int hslua_get_caching_table(lua_State *L, int index);

/* Get a value from the object's uservalue cache */
int hslua_get_from_cache(lua_State *L);

#endif
