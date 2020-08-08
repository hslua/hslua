#define HSLUA_ERR "HSLUA_ERR"
#define HSLUA_HS_FUN_NAME "HsLuaFunction"

/*  exported from Foreign.Lua.Raw.Call */
int hslua_call_wrapped_hs_fun(lua_State *L);

/*  register metatable for HaskellFunction userdata wrappers */
void hslua_registerhsfunmetatable(lua_State *L);
