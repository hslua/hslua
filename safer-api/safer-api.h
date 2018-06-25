#include "lua.h"

int hslua_call_hs(lua_State *L);

int hslua_userdata_gc(lua_State *L);

#if LUA_VERSION_NUM >= 502
int hslua_compare(lua_State *L, int index1, int index2, int op);
#endif

int hslua_concat(lua_State *L, int n);

int hslua_getfield(lua_State *L, int index, const char *k);

int hslua_getglobal(lua_State *L, const char *name);

int hslua_gettable(lua_State *L, int index);

int hslua_setfield(lua_State *L, int index, const char *k);

int hslua_setglobal(lua_State *L, const char *k);

int hslua_settable(lua_State *L, int index);

int hslua_next(lua_State *L, int index);
