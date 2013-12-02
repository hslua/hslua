/*
** See Copyright Notice in lua.h
*/

#include <ctype.h>
#include <stdio.h>

#define luac_c
#define LUA_CORE

#include "lua.h"
#include "lauxlib.h"

#include "ntrljmp.h"

extern int hsmethod__call( lua_State *state );

LUAI_FUNC int lua_neutralize_longjmp( lua_State *state )
{
    int result;
    result = hsmethod__call(state);
        
    switch(result) {
      case HSLUA_LONGJMP_ERROR:
        return lua_error(state);
      break;
      
      case HSLUA_LONGJMP_ARG_ERROR: 
      {
        const char *msg = lua_tostring(state, -1);
        int narg = lua_tointeger(state, -2);
       
        lua_pop(state, 2);
        return luaL_argerror(state, narg, msg);
      }
      break;
      default:
      break;
    }
      
    return result;
}
