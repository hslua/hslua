/*
** Lua virtual machine
** See Copyright Notice in lua.h
*/

#ifndef lvm_h
#define lvm_h


#define luac_c
#define LUA_CORE

#include "lua.h"
#include "lobject.h"
#include "ltm.h"

LUAI_FUNC int lua_neutralize_longjmp(lua_State *state);

#endif
