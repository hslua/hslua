/*
** See Copyright Notice in lua.h
*/

#include <ctype.h>

#define luac_c
#define LUA_CORE

#include "lua.h"

extern int hsmethod__call( lua_State *state );

extern int lua_neutralize_longjmp( lua_State *state )
{
    int result;
    result = hsmethod__call(state);
    if (result < 0)
    {
        lua_getglobal(state, "_HASKELLERR");
        lua_insert(state, -2);
        return 2;
    }
    return result;
}
