/*
** Large parts of the below code are adapted from the default warning
** functions defined in lauxlib.c.
**
** See Copyright Notice in lua.h
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"
#include "lauxlib.h"

#define HSLUA_RFLD_WARNF "HsLua warn hook"
#define HSLUA_RFLD_WARNINGS "HsLua warnings"

/* print an error message */
#if !defined(lua_writestringerror)
#define lua_writestringerror(s,p)               \
  (fprintf(stderr, (s), (p)), fflush(stderr))
#endif

/*
** Initializes the warnings table.
*/
static void reset_warnings (lua_State *L) {
  lua_createtable(L, 1, 0);
  lua_setfield(L, LUA_REGISTRYINDEX, HSLUA_RFLD_WARNINGS);
}

/*
** Stores the warning in the registry.
*/
static void store_warning (lua_State *L, const char *message) {
  if (lua_getfield(L, LUA_REGISTRYINDEX, HSLUA_RFLD_WARNINGS) != LUA_TTABLE) {
    return;
  }
  lua_pushstring(L, message);
  lua_seti(L, -2, luaL_len(L, -2) + 1);  /* append message */
  lua_pop(L, 1);  /* messages table */
}

/* Concatenates all collected warnings and pushes the result to the stack. */
static void pushwarning (lua_State *L) {
  if (lua_getfield(L, LUA_REGISTRYINDEX, HSLUA_RFLD_WARNINGS) != LUA_TTABLE) {
    lua_pushliteral(L, "");
    return;
  }

  int tblidx = lua_absindex(L, -1);
  lua_Integer last = luaL_len(L, tblidx);
  luaL_Buffer b;
  luaL_buffinit(L, &b);
  for (int i = 1; i <= last; i++) {
    if (lua_geti(L, tblidx, i) != LUA_TSTRING) {
      /* not a string; skip it silently */
      lua_pop(L, 1);
    } else {
      luaL_addvalue(&b);
    }
  }
  lua_remove(L, -2);  /* warnings table */
  luaL_pushresult(&b);
}

static void call_warn_hook (lua_State *L) {
  if (lua_getfield(L, LUA_REGISTRYINDEX, HSLUA_RFLD_WARNF) == LUA_TFUNCTION) {
    pushwarning(L);
    lua_call(L, 1, 0);
  }
}

/*
** Warning functions:
** warnfoff: warning system is off
** warnfon: ready to start a new message
** warnfcont: previous message is to be continued
*/
static void warnfoff (void *ud, const char *message, int tocont);
static void warnfon (void *ud, const char *message, int tocont);
static void warnfcont (void *ud, const char *message, int tocont);

/*
** Check whether message is a control message. If so, execute the
** control or ignore it if unknown.
*/
static int checkcontrol (lua_State *L, const char *message, int tocont) {
  if (tocont || *(message++) != '@')  /* not a control message? */
    return 0;
  else {
    if (strcmp(message, "off") == 0)
      lua_setwarnf(L, warnfoff, L);  /* turn warnings off */
    else if (strcmp(message, "on") == 0)
      lua_setwarnf(L, warnfon, L);   /* turn warnings on */
    return 1;  /* it was a control message */
  }
}

/*
** Does not write the warning to stderr, but still records the message
** so it can be processed by the custom hook.
*/
static void warnfoff (void *ud, const char *message, int tocont) {
  lua_State *L = (lua_State *)ud;
  if (checkcontrol(L, message, tocont)) {
    return; /* nothing else to be done */
  }
  store_warning(L, message);
  if (!tocont) {  /* last part */
    call_warn_hook(L);  /* call the warnings hook */
    reset_warnings(L);  /* reset warnings table */
  }
}


/*
** Writes the message and handle 'tocont', finishing the message
** if needed and setting the next warn function.
*/
static void warnfcont (void *ud, const char *message, int tocont) {
  lua_State *L = (lua_State *)ud;
  lua_writestringerror("%s", message);  /* write message */
  store_warning(L, message);
  if (tocont)  /* not the last part? */
    lua_setwarnf(L, warnfcont, L);  /* to be continued */
  else {  /* last part */
    lua_writestringerror("%s", "\n");  /* finish message with end-of-line */
    lua_setwarnf(L, warnfon, L);  /* next call is a new message */
    call_warn_hook(L);  /* call the warnings hook */
    reset_warnings(L);  /* reset warnings table */
  }
}

/*
** Records a warning, and writes a warning prefix followed by the
** warning to stderr.
*/
static void warnfon (void *ud, const char *message, int tocont) {
  lua_State *L = (lua_State *)ud;
  if (checkcontrol(L, message, tocont))  /* control message? */
    return;  /* nothing else to be done */
  lua_writestringerror("%s", "Lua warning: ");  /* start a new warning */
  warnfcont(ud, message, tocont);  /* finish processing */
}


/*
** Sets the object at the top of the stack as the function that is
** called on the concatenated warning messages. Pops the function from
** the stack.
*/
void hsluaL_setwarnf (lua_State *L) {
  lua_setfield(L, LUA_REGISTRYINDEX, HSLUA_RFLD_WARNF);
  reset_warnings(L);
  lua_setwarnf(L, warnfoff, L);
}
