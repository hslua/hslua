{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Core.RawBindings
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Haskell bindings to lua C API functions.
-}
module Foreign.Lua.Core.RawBindings where

import Foreign.C
import Foreign.Lua.Core.Error (Failable (Failable))
import Foreign.Lua.Core.Types as Lua
import Foreign.Ptr

##ifdef ALLOW_UNSAFE_GC
##define SAFTY unsafe
##else
##define SAFTY safe
##endif

-- TODO: lua_getallocf, lua_setallocf
-- TODO: Debugger functions

-- Some of the Lua functions may call a Haskell function, and trigger
-- garbage collection, rescheduling etc. This means we must declare these
-- functions as 'safe'.


--------------------------------------------------------------------------------
-- * State manipulation

-- lua_newstate is currently not supported.

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_close lua_close>
foreign import ccall "lua.h lua_close"
  lua_close :: Lua.State -> IO ()

-- lua_newthread is currently not supported.


--------------------------------------------------------------------------------
-- * Basic stack manipulation

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_absindex lua_absindex>
foreign import ccall unsafe "lua.h lua_absindex"
  lua_absindex :: Lua.State -> StackIndex -> IO StackIndex

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gettop lua_gettop>
foreign import ccall unsafe "lua.h lua_gettop"
  lua_gettop :: Lua.State -> IO StackIndex

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_settop lua_settop>
foreign import ccall SAFTY "lua.h lua_settop"
  lua_settop :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushvalue lua_pushvalue>
foreign import ccall SAFTY "lua.h lua_pushvalue"
  lua_pushvalue :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_copy lua_copy>
foreign import ccall SAFTY "lua.h lua_copy"
  lua_copy :: Lua.State -> StackIndex -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_remove lua_remove>
foreign import capi SAFTY "lua.h lua_remove"
  lua_remove :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_insert lua_insert>
foreign import capi SAFTY "lua.h lua_insert"
  lua_insert :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_replace lua_replace>
foreign import capi SAFTY "lua.h lua_replace"
  lua_replace :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_checkstack lua_checkstack>
foreign import capi SAFTY "lua.h lua_checkstack"
  lua_checkstack :: Lua.State -> CInt -> IO LuaBool

-- lua_xmove is currently not supported.


--------------------------------------------------------------------------------
-- * Stack access functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnumber lua_isnumber>
foreign import ccall SAFTY "lua.h lua_isnumber"
  lua_isnumber :: Lua.State -> StackIndex -> IO LuaBool

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isstring lua_isstring>
foreign import ccall SAFTY "lua.h lua_isstring"
  lua_isstring :: Lua.State -> StackIndex -> IO LuaBool

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_iscfunction lua_iscfunction>
foreign import ccall SAFTY "lua.h lua_iscfunction"
  lua_iscfunction :: Lua.State -> StackIndex -> IO LuaBool

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isuserdata lua_isuserdata>
foreign import ccall SAFTY "lua.h lua_isuserdata"
  lua_isuserdata :: Lua.State -> StackIndex -> IO LuaBool

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>
foreign import ccall SAFTY "lua.h lua_type"
  lua_type :: Lua.State -> StackIndex -> IO TypeCode

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_typename lua_typename>
foreign import ccall SAFTY "lua.h lua_typename"
  lua_typename :: Lua.State -> TypeCode -> IO CString

-- lua_compare is unsafe (might cause a longjmp), use hslua_compare instead.

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_compare \
-- @lua_compare@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_compare"
  hslua_compare :: Lua.State -> StackIndex -> StackIndex -> CInt
                -> IO (Failable LuaBool)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawequal lua_rawequal>
foreign import ccall SAFTY "lua.h lua_rawequal"
  lua_rawequal :: Lua.State -> StackIndex -> StackIndex -> IO LuaBool

--
-- Type coercion
--
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_toboolean lua_toboolean>
foreign import capi SAFTY "lua.h lua_toboolean"
  lua_toboolean :: Lua.State -> StackIndex -> IO LuaBool

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction lua_tocfunction>
foreign import ccall SAFTY "lua.h lua_tocfunction"
  lua_tocfunction :: Lua.State -> StackIndex -> IO CFunction

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tointegerx lua_tointegerx>
foreign import ccall SAFTY "lua.h lua_tointegerx"
  lua_tointegerx :: Lua.State -> StackIndex -> Ptr LuaBool -> IO Lua.Integer

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tonumberx lua_tonumberx>
foreign import ccall SAFTY "lua.h lua_tonumberx"
  lua_tonumberx :: Lua.State -> StackIndex -> Ptr LuaBool -> IO Lua.Number

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tolstring lua_tolstring>
foreign import ccall SAFTY "lua.h lua_tolstring"
  lua_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_topointer lua_topointer>
foreign import ccall SAFTY "lua.h lua_topointer"
  lua_topointer :: Lua.State -> StackIndex -> IO (Ptr ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tothread lua_tothread>
foreign import ccall SAFTY "lua.h lua_tothread"
  lua_tothread :: Lua.State -> StackIndex -> IO Lua.State

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_touserdata lua_touserdata>
foreign import ccall SAFTY "lua.h lua_touserdata"
  lua_touserdata :: Lua.State -> StackIndex -> IO (Ptr a)


--
-- Object size
--

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawlen lua_rawlen>
foreign import ccall SAFTY "lua.h lua_rawlen"
  lua_rawlen :: Lua.State -> StackIndex -> IO CSize


--------------------------------------------------------------------------------
-- * Push functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnil lua_pushnil>
foreign import ccall SAFTY "lua.h lua_pushnil"
  lua_pushnil :: Lua.State -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnumber lua_pushnumber>
foreign import ccall SAFTY "lua.h lua_pushnumber"
  lua_pushnumber :: Lua.State -> Lua.Number -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushinteger lua_pushinteger>
foreign import ccall SAFTY "lua.h lua_pushinteger"
  lua_pushinteger :: Lua.State -> Lua.Integer -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushlstring lua_pushlstring>
foreign import ccall SAFTY "lua.h lua_pushlstring"
  lua_pushlstring :: Lua.State -> Ptr CChar -> CSize -> IO ()

-- lua_pushstring is currently not supported. It's difficult to use in a haskell
-- context.

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushcclosure lua_pushcclosure>
foreign import ccall SAFTY "lua.h lua_pushcclosure"
  lua_pushcclosure :: Lua.State -> CFunction -> NumArgs -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushboolean lua_pushboolean>
foreign import ccall SAFTY "lua.h lua_pushboolean"
  lua_pushboolean :: Lua.State -> LuaBool -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushlightuserdata lua_pushlightuserdata>
foreign import ccall SAFTY "lua.h lua_pushlightuserdata"
  lua_pushlightuserdata :: Lua.State -> Ptr a -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushthread lua_pushthread>
foreign import ccall SAFTY "lua.h lua_pushthread"
  lua_pushthread :: Lua.State -> IO CInt


--------------------------------------------------------------------------------
-- * Get functions

-- + lua_gettable is unsafe, use hslua_gettable instead.
-- + lua_getglobal is unsafe, use hslua_getglobal instead.
-- + lua_getfield is unsafe, we build something equivallent using pushlstring and
--   gettable.

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_gettable \
-- @lua_gettable@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_gettable"
  hslua_gettable :: Lua.State -> StackIndex -> IO (Failable ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawget lua_rawget>
foreign import ccall SAFTY "lua.h lua_rawget"
  lua_rawget :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti lua_rawgeti>
foreign import ccall SAFTY "lua.h lua_rawgeti"
  lua_rawgeti :: Lua.State -> StackIndex -> Lua.Integer -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_createtable lua_createtable>
foreign import ccall SAFTY "lua.h lua_createtable"
  lua_createtable :: Lua.State -> CInt -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newuserdata lua_newuserdata>
foreign import ccall SAFTY "lua.h lua_newuserdata"
  lua_newuserdata :: Lua.State -> CSize -> IO (Ptr ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getmetatable lua_getmetatable>
foreign import ccall SAFTY "lua.h lua_getmetatable"
  lua_getmetatable :: Lua.State -> StackIndex -> IO LuaBool

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_getglobal \
-- @lua_getglobal@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_getglobal"
  hslua_getglobal :: Lua.State -> CString -> CSize -> IO (Failable ())


--------------------------------------------------------------------------------
-- * Set functions

-- lua_settable is unsafe, use hslua_settable instead.
-- lua_setfield is unsafe, use hslua_setfield instead.
-- lua_setglobal is unsafe, use hslua_setglobal instead.
-- lua_setfenv (5.1 only) is not supported.

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_settable \
-- @lua_settable@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_settable"
  hslua_settable :: Lua.State -> StackIndex -> IO (Failable ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawset lua_rawset>
foreign import ccall SAFTY "lua.h lua_rawset"
  lua_rawset :: Lua.State -> StackIndex -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawseti lua_rawseti>
foreign import ccall SAFTY "lua.h lua_rawseti"
  lua_rawseti :: Lua.State -> StackIndex -> Lua.Integer -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setmetatable lua_setmetatable>
foreign import ccall SAFTY "lua.h lua_setmetatable"
  lua_setmetatable :: Lua.State -> StackIndex -> IO ()

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_setglobal \
-- @lua_setglobal@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_setglobal"
  hslua_setglobal :: Lua.State -> CString -> CSize -> IO (Failable ())


--------------------------------------------------------------------------------
-- * 'load' and 'call' functions (load and run Lua code)

-- lua_call is inherently unsafe, we do not support it.

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pcall lua_pcall>
foreign import capi "lua.h lua_pcall"
  lua_pcall :: Lua.State -> NumArgs -> NumResults -> StackIndex
            -> IO StatusCode

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_load lua_load>
foreign import ccall safe "lua.h lua_load"
  lua_load :: Lua.State -> Lua.Reader -> Ptr () -> CString -> CString
           -> IO StatusCode

-- currently unsupported:
-- lua_dump


------------------------------------------------------------------------------
-- * Coroutine functions

-- lua_yield / lua_yieldk and lua_resume are currently not supported.

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_status lua_status>
foreign import ccall unsafe "lua.h lua_status"
  lua_status :: Lua.State -> IO StatusCode


------------------------------------------------------------------------------
-- * Garbage-collection functions and options

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gc lua_gc>
foreign import ccall "lua.h lua_gc"
  lua_gc :: Lua.State -> CInt -> CInt -> IO CInt


------------------------------------------------------------------------------
-- * Miscellaneous functions

-- lua_error is unsafe in a haskell context and hence not supported.
-- lua_next is unsafe, use hslua_next instead.
-- lua_concat is unsafe (may trigger a longjmp), use hslua_concat instead.

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_next \
-- @lua_next@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_next"
  hslua_next :: Lua.State -> StackIndex -> IO (Failable LuaBool)

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_concat \
-- @lua_concat@> which catches any @longjmp@s.
foreign import ccall "error-conversion.h hslua_concat"
  hslua_concat :: Lua.State -> NumArgs -> IO (Failable ())


------------------------------------------------------------------------------
-- * Lua Libraries

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_openlibs luaL_openlibs>
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
