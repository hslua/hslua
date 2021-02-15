{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Functions
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, CPP

Haskell bindings to Lua C API functions.

The exposed functions correspond closely to the respective C Lua API
functions. However, C API functions which can throw Lua errors are not
exported directly, as any errors would crash the program. Non-error
throwing @hslua_@ versions are provided instead. The @hslua@ ersatz
functions have worse performance than the original.

Some of the Lua functions may, directly or indirectly, call a Haskell
function, and trigger garbage collection, rescheduling etc. These
functions are always imported safely (i.e., with the @safe@ keyword).

However, all function can trigger garbage collection. If that can lead
to problems, then the package should be configured without flag
@allow-unsafe-gc@.
-}
module Foreign.Lua.Raw.Functions where

import Foreign.C
import Foreign.Lua.Raw.Types as Lua
import Foreign.Ptr

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- * State manipulation

-- | Wrapper of @lua_close@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_close>.
foreign import ccall safe "lua.h lua_close"
  lua_close :: Lua.State -> IO ()


-- * Basic stack manipulation

-- | Wrapper of @lua_absindex@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_absindex>.
foreign import ccall SAFTY "lua.h lua_absindex"
  lua_absindex :: Lua.State -> StackIndex -> IO StackIndex

-- | Wrapper of @lua_gettop@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_gettop>.
foreign import ccall SAFTY "lua.h lua_gettop"
  lua_gettop :: Lua.State -> IO StackIndex

-- | Wrapper of @lua_settop@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_settop>.
foreign import ccall SAFTY "lua.h lua_settop"
  lua_settop :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_pushvalue@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushvalue>.
foreign import ccall SAFTY "lua.h lua_pushvalue"
  lua_pushvalue :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_pop@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pop>.
foreign import capi SAFTY "lua.h lua_pop"
  lua_pop :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_copy@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_copy>.
foreign import ccall SAFTY "lua.h lua_copy"
  lua_copy :: Lua.State -> StackIndex -> StackIndex -> IO ()

-- | Wrapper of @lua_remove@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_remove>.
foreign import capi SAFTY "lua.h lua_remove"
  lua_remove :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_insert@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_insert>.
foreign import capi SAFTY "lua.h lua_insert"
  lua_insert :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_replace@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_replace>.
foreign import capi SAFTY "lua.h lua_replace"
  lua_replace :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_checkstack@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_checkstack>.
foreign import capi SAFTY "lua.h lua_checkstack"
  lua_checkstack :: Lua.State -> CInt -> IO LuaBool


-- * Access functions (stack -> Haskell)

-- | Wrapper of @lua_isnumber@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_isnumber>.
foreign import ccall SAFTY "lua.h lua_isnumber"
  lua_isnumber :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_isinteger@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_isinteger>.
foreign import ccall SAFTY "lua.h lua_isinteger"
  lua_isinteger :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_isstring@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_isstring>.
foreign import ccall SAFTY "lua.h lua_isstring"
  lua_isstring :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_iscfunction@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_iscfunction>.
foreign import ccall SAFTY "lua.h lua_iscfunction"
  lua_iscfunction :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_isuserdata@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_isuserdata>.
foreign import ccall SAFTY "lua.h lua_isuserdata"
  lua_isuserdata :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_type@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_type>.
foreign import ccall SAFTY "lua.h lua_type"
  lua_type :: Lua.State -> StackIndex -> IO TypeCode

-- | Wrapper of @lua_typename@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_typename>.
foreign import ccall SAFTY "lua.h lua_typename"
  lua_typename :: Lua.State -> TypeCode -> IO CString

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_compare \
-- @lua_compare@> which catches any Lua errors.
foreign import capi safe "hslua.h hslua_compare"
  hslua_compare :: Lua.State
                -> StackIndex -> StackIndex -> CInt
                -> Ptr StatusCode -> IO LuaBool

-- | Wrapper of @lua_rawequal@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawequal>.
foreign import ccall SAFTY "lua.h lua_rawequal"
  lua_rawequal :: Lua.State -> StackIndex -> StackIndex -> IO LuaBool


-- | Wrapper of @lua_toboolean@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_toboolean>.
foreign import capi SAFTY "lua.h lua_toboolean"
  lua_toboolean :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper of @lua_tocfunction@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction>.
foreign import ccall SAFTY "lua.h lua_tocfunction"
  lua_tocfunction :: Lua.State -> StackIndex -> IO CFunction

-- | Wrapper of @lua_tointegerx@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_tointegerx>.
foreign import ccall SAFTY "lua.h lua_tointegerx"
  lua_tointegerx :: Lua.State -> StackIndex -> Ptr LuaBool -> IO Lua.Integer

-- | Wrapper of @lua_tonumberx@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_tonumberx>.
foreign import ccall SAFTY "lua.h lua_tonumberx"
  lua_tonumberx :: Lua.State -> StackIndex -> Ptr LuaBool -> IO Lua.Number

-- | Wrapper of @lua_tolstring@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_tolstring>.
foreign import ccall SAFTY "lua.h lua_tolstring"
  lua_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

-- | Wrapper of @lua_topointer@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_topointer>.
foreign import ccall SAFTY "lua.h lua_topointer"
  lua_topointer :: Lua.State -> StackIndex -> IO (Ptr ())

-- | Wrapper of @lua_tothread@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_tothread>.
foreign import ccall SAFTY "lua.h lua_tothread"
  lua_tothread :: Lua.State -> StackIndex -> IO Lua.State

-- | Wrapper of @lua_touserdata@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_touserdata>.
foreign import ccall SAFTY "lua.h lua_touserdata"
  lua_touserdata :: Lua.State -> StackIndex -> IO (Ptr a)


-- | Wrapper of @lua_rawlen@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawlen>.
foreign import ccall SAFTY "lua.h lua_rawlen"
  lua_rawlen :: Lua.State -> StackIndex -> IO CSize


-- * Push functions (Haskell -> stack)

-- | Wrapper of @lua_pushnil@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushnil>.
foreign import ccall SAFTY "lua.h lua_pushnil"
  lua_pushnil :: Lua.State -> IO ()

-- | Wrapper of @lua_pushnumber@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushnumber>.
foreign import ccall SAFTY "lua.h lua_pushnumber"
  lua_pushnumber :: Lua.State -> Lua.Number -> IO ()

-- | Wrapper of @lua_pushinteger@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushinteger>.
foreign import ccall SAFTY "lua.h lua_pushinteger"
  lua_pushinteger :: Lua.State -> Lua.Integer -> IO ()

-- | Wrapper of @lua_pushlstring@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushlstring>.
foreign import ccall SAFTY "lua.h lua_pushlstring"
  lua_pushlstring :: Lua.State -> Ptr CChar -> CSize -> IO ()

-- | Wrapper of @lua_pushcclosure@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushcclosure>.
foreign import ccall SAFTY "lua.h lua_pushcclosure"
  lua_pushcclosure :: Lua.State -> CFunction -> NumArgs -> IO ()

-- | Wrapper of @lua_pushboolean@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushboolean>.
foreign import ccall SAFTY "lua.h lua_pushboolean"
  lua_pushboolean :: Lua.State -> LuaBool -> IO ()

-- | Wrapper of @lua_pushlightuserdata@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushlightuserdata>.
foreign import ccall SAFTY "lua.h lua_pushlightuserdata"
  lua_pushlightuserdata :: Lua.State -> Ptr a -> IO ()

-- | Wrapper of @lua_pushthread@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushthread>.
foreign import ccall SAFTY "lua.h lua_pushthread"
  lua_pushthread :: Lua.State -> IO CInt


-- * Get functions (Lua -> stack)

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>.
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_gettable"
  hslua_gettable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO ()

-- | Wrapper of @lua_rawget@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawget>.
foreign import ccall SAFTY "lua.h lua_rawget"
  lua_rawget :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_rawgeti@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti>.
foreign import ccall SAFTY "lua.h lua_rawgeti"
  lua_rawgeti :: Lua.State -> StackIndex -> Lua.Integer -> IO ()

-- | Wrapper of @lua_createtable@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_createtable>.
foreign import ccall SAFTY "lua.h lua_createtable"
  lua_createtable :: Lua.State -> CInt -> CInt -> IO ()

-- | Wrapper of @lua_newuserdata@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_newuserdata>.
foreign import ccall SAFTY "lua.h lua_newuserdata"
  lua_newuserdata :: Lua.State -> CSize -> IO (Ptr ())

-- | Wrapper of @lua_getmetatable@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_getmetatable>.
foreign import ccall SAFTY "lua.h lua_getmetatable"
  lua_getmetatable :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_getglobal"
  hslua_getglobal :: Lua.State -> CString -> CSize -> Ptr StatusCode -> IO ()


-- * Set functions (stack -> Lua)

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_settable \
-- @lua_settable@> which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_settable"
  hslua_settable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO ()

-- | Wrapper of @lua_rawset@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawset>.
foreign import ccall SAFTY "lua.h lua_rawset"
  lua_rawset :: Lua.State -> StackIndex -> IO ()

-- | Wrapper of @lua_rawseti@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawseti>.
foreign import ccall SAFTY "lua.h lua_rawseti"
  lua_rawseti :: Lua.State -> StackIndex -> Lua.Integer -> IO ()

-- | Wrapper of @lua_setmetatable@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_setmetatable>.
foreign import ccall SAFTY "lua.h lua_setmetatable"
  lua_setmetatable :: Lua.State -> StackIndex -> IO ()

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_setglobal \
-- @lua_setglobal@> which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_setglobal"
  hslua_setglobal :: Lua.State -> CString -> CSize -> Ptr StatusCode -> IO ()


-- * \'load\' and \'call\' functions (load and run Lua code)

-- | Wrapper of @lua_pcall@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pcall>.
foreign import capi safe "lua.h lua_pcall"
  lua_pcall :: Lua.State -> NumArgs -> NumResults -> StackIndex
            -> IO StatusCode

-- | Wrapper of @lua_load@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_load>.
foreign import ccall safe "lua.h lua_load"
  lua_load :: Lua.State -> Lua.Reader -> Ptr () -> CString -> CString
           -> IO StatusCode


-- * Coroutine functions

-- | Wrapper of @lua_status@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_status>.
foreign import ccall SAFTY "lua.h lua_status"
  lua_status :: Lua.State -> IO StatusCode


-- * Garbage-collection functions and options

-- | Wrapper of @lua_gc@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_gc>.
foreign import ccall safe "lua.h lua_gc"
  lua_gc :: Lua.State -> CInt -> CInt -> IO CInt


-- * Miscellaneous functions

-- | Replacement for
-- <https://lua.org/manual/5.3/manual.html#lua_error lua_error>; it uses
-- the HsLua error signaling convention instead of raw Lua errors.
foreign import ccall SAFTY "hslua.h hslua_error"
  hslua_error :: Lua.State -> IO NumResults

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_next lua_next> which
-- catches any Lua errors.
foreign import ccall safe "hslua.h hslua_next"
  hslua_next :: Lua.State -> StackIndex -> Ptr StatusCode -> IO LuaBool

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_concat lua_concat>
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_concat"
  hslua_concat :: Lua.State -> NumArgs -> Ptr StatusCode -> IO ()

-- | Wrapper of @lua_pushglobaltable@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushglobaltable>.
foreign import capi SAFTY "lua.h lua_pushglobaltable"
  lua_pushglobaltable :: Lua.State -> IO ()


-- * Lua Libraries

-- | Wrapper of @luaL_openlibs@. See the Lua docs at
-- <https://www.lua.org/manual/5.3/manual.html#luaL_openlibs>.
foreign import ccall unsafe "lualib.h luaL_openlibs"
  luaL_openlibs :: Lua.State -> IO ()

-- | Point to function opening the base library.
foreign import ccall unsafe "lualib.h &luaopen_base"
  lua_open_base_ptr :: CFunction

-- | Point to function opening the table library.
foreign import ccall unsafe "lualib.h &luaopen_table"
  lua_open_table_ptr :: CFunction

-- | Point to function opening the io library.
foreign import ccall unsafe "lualib.h &luaopen_io"
  lua_open_io_ptr :: CFunction

-- | Point to function opening the os library.
foreign import ccall unsafe "lualib.h &luaopen_os"
  lua_open_os_ptr :: CFunction

-- | Point to function opening the string library.
foreign import ccall unsafe "lualib.h &luaopen_string"
  lua_open_string_ptr :: CFunction

-- | Point to function opening the math library.
foreign import ccall unsafe "lualib.h &luaopen_math"
  lua_open_math_ptr :: CFunction

-- | Point to function opening the debug library.
foreign import ccall unsafe "lualib.h &luaopen_debug"
  lua_open_debug_ptr :: CFunction

-- | Point to function opening the package library.
foreign import ccall unsafe "lualib.h &luaopen_package"
  lua_open_package_ptr :: CFunction
