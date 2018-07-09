#include "lua.h"

int hslua_getlfield(lua_State *L, int index, const char *k, size_t len);

int hslua_setfield(lua_State *L, int index, const char *k);
