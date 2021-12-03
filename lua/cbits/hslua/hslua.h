#include <lua.h>

/* Replacement for lua_error, signals an error in a function */
int hslua_error(lua_State *L);

/* Converts a Haskell function into a lua_CFunction. */
int hslua_call_hs(lua_State *L);

/* Ersatz functions */
void hslua_arith(lua_State *L, int op, int *status);
int hslua_compare(lua_State *L, int index1, int index2, int op, int *status);
void hslua_concat(lua_State *L, int n, int *status);
int hslua_getglobal(lua_State *L, const char *name, size_t len, int *status);
int hslua_gettable(lua_State *L, int index, int *status);
int hslua_next(lua_State *L, int index, int *status);
void hslua_setglobal(lua_State *L, const char *k, size_t len, int *status);
void hslua_settable(lua_State *L, int index, int *status);
