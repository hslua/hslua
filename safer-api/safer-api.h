#include "lua.h"
#include "lauxlib.h"

int hslua_call_hs(lua_State *L);

int hslua_userdata_gc(lua_State *L);

int hslua_compare(lua_State *L, int index1, int index2, int op);

int hslua_concat(lua_State *L, int n);

int hslua_getglobal(lua_State *L, const char *name, size_t len);

int hslua_gettable(lua_State *L, int index);

int hslua_setglobal(lua_State *L, const char *k, size_t len);

int hslua_settable(lua_State *L, int index);

int hslua_next(lua_State *L, int index);


/* auxiliary library */
const char *hsluaL_tolstring(lua_State *L, int index, size_t *len);
