#ifndef hslcall_h
#define hslcall_h

#define HSLUA_ERR "HSLUA_ERR"
#define HSLUA_HSFUN_NAME "HsLuaFunction"
#include <lua.h>
#include <HsFFI.h>

/*  register metatable for HaskellFunction userdata wrappers */
void hslua_registerhsfunmetatable(lua_State *L);

#endif
