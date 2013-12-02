#ifndef _NTRLJMP_H
#define _NTRLJMP_H


#include "lua.h"

#define HSLUA_LONGJMP_ERROR -1
#define HSLUA_LONGJMP_ARG_ERROR -2

LUAI_FUNC int lua_neutralize_longjmp(lua_State *state);

#endif  //_NTRLJMP_H
