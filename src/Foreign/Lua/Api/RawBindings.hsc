{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Foreign.Lua.Api.RawBindings
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Haskell bindings to lua C API functions.
-}
module Foreign.Lua.Api.RawBindings where

import Foreign.C
import Foreign.Lua.Types.Core
import Foreign.Ptr

#include "lua.h"

-- TODO: lua_getallocf, lua_setallocf
-- TODO: Debugger functions

-- Some of the Lua functions may call a Haskell function, and trigger
-- garbage collection, rescheduling etc. This means we must declare these
-- functions as 'safe'.
--
-- FIXME: Currently everthing marked as 'safe'. Change the ones that don't
-- call Haskell to 'unsafe'.


--------------------------------------------------------------------------------
-- * State manipulation

foreign import ccall "lua.h lua_newstate"
  lua_newstate :: FunPtr LuaAlloc -> Ptr () -> IO LuaState

foreign import ccall "lua.h lua_close"
  lua_close :: LuaState -> IO ()

foreign import ccall "lua.h lua_newthread"
  lua_newthread :: LuaState -> IO LuaState

foreign import ccall "lua.h lua_atpanic"
  lua_atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)


--------------------------------------------------------------------------------
-- * Basic stack manipulation

foreign import ccall "lua.h lua_gettop"
  lua_gettop :: LuaState -> IO CInt

foreign import ccall "lua.h lua_settop"
  lua_settop :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushvalue"
  lua_pushvalue :: LuaState -> CInt -> IO ()

#if LUA_VERSION_NUMBER >= 503
foreign import ccall "lua.h lua_rotate"
  lua_rotate :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_copy"
  lua_copy :: LuaState -> CInt -> CInt -> IO ()
#else
foreign import ccall "lua.h lua_remove"
  lua_remove :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_insert"
  lua_insert :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_replace"
  lua_replace :: LuaState -> CInt -> IO ()
#endif

foreign import ccall "lua.h lua_checkstack"
  lua_checkstack :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_xmove"
  lua_xmove :: LuaState -> LuaState -> CInt -> IO ()


--------------------------------------------------------------------------------
-- * Stack access functions

foreign import ccall "lua.h lua_isnumber"
  lua_isnumber :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isstring"
  lua_isstring :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_iscfunction"
  lua_iscfunction :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isuserdata"
  lua_isuserdata :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_type"
  lua_type :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_typename"
  lua_typename :: LuaState -> CInt -> IO (Ptr CChar)

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_compare"
  lua_compare :: LuaState -> CInt -> CInt -> CInt -> IO CInt
#else
foreign import ccall "lua.h lua_equal"
  lua_equal :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_lessthan"
  lua_lessthan :: LuaState -> CInt -> CInt -> IO CInt
#endif

foreign import ccall "lua.h lua_rawequal"
  lua_rawequal :: LuaState -> CInt -> CInt -> IO CInt

--
-- Type coercion
--
foreign import ccall "lua.h lua_toboolean"
  lua_toboolean :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_tocfunction"
  lua_tocfunction :: LuaState -> CInt -> IO (FunPtr LuaCFunction)

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_tointegerx"
  lua_tointegerx :: LuaState -> CInt -> CInt -> IO LuaInteger

foreign import ccall "lua.h lua_tonumberx"
  lua_tonumberx :: LuaState -> CInt -> CInt -> IO LuaNumber
#else
foreign import ccall "lua.h lua_tointeger"
  lua_tointeger :: LuaState -> CInt -> IO LuaInteger

foreign import ccall "lua.h lua_tonumber"
  lua_tonumber :: LuaState -> CInt -> IO LuaNumber
#endif

foreign import ccall "lua.h lua_tolstring"
  lua_tolstring :: LuaState -> CInt -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall "lua.h lua_topointer"
  lua_topointer :: LuaState -> CInt -> IO (Ptr ())

foreign import ccall "lua.h lua_tothread"
  lua_tothread :: LuaState -> CInt -> IO LuaState

foreign import ccall "lua.h lua_touserdata"
  lua_touserdata :: LuaState -> CInt -> IO (Ptr a)


--
-- Object size
--

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_rawlen"
  lua_rawlen :: LuaState -> CInt -> IO CSize
#else
foreign import ccall "lua.h lua_objlen"
  lua_objlen :: LuaState -> CInt -> IO CSize
#endif


--------------------------------------------------------------------------------
-- * Push functions

foreign import ccall "lua.h lua_pushnil"
  lua_pushnil :: LuaState -> IO ()

foreign import ccall "lua.h lua_pushnumber"
  lua_pushnumber :: LuaState -> LuaNumber -> IO ()

foreign import ccall "lua.h lua_pushinteger"
  lua_pushinteger :: LuaState -> LuaInteger -> IO ()

foreign import ccall "lua.h lua_pushlstring"
  lua_pushlstring :: LuaState -> Ptr CChar -> CSize -> IO ()

foreign import ccall "lua.h lua_pushstring"
  lua_pushstring :: LuaState -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_pushcclosure"
  lua_pushcclosure :: LuaState -> FunPtr LuaCFunction -> CInt -> IO ()

foreign import ccall "lua.h lua_pushboolean"
  lua_pushboolean :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushlightuserdata"
  lua_pushlightuserdata :: LuaState -> Ptr a -> IO ()

foreign import ccall "lua.h lua_pushthread"
  lua_pushthread :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Get functions

#if LUA_VERSION_NUMBER >= 503
foreign import ccall "lua.h lua_gettable"
  lua_gettable :: LuaState -> CInt -> IO CInt
#else
foreign import ccall "lua.h lua_gettable"
  lua_gettable :: LuaState -> CInt -> IO ()
#endif

foreign import ccall "lua.h lua_getfield"
  lua_getfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawget"
  lua_rawget :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawgeti"
  lua_rawgeti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_createtable"
  lua_createtable :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_newuserdata"
  lua_newuserdata :: LuaState -> CInt -> IO (Ptr ())

foreign import ccall "lua.h lua_getmetatable"
  lua_getmetatable :: LuaState -> CInt -> IO CInt

#if LUA_VERSION_NUMBER < 502
foreign import ccall "lua.h lua_getfenv"
  lua_getfenv :: LuaState -> CInt -> IO ()
#endif

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_getglobal"
  lua_getglobal :: LuaState -> Ptr CChar -> IO ()
#endif

--------------------------------------------------------------------------------
-- * Set functions

foreign import ccall "lua.h lua_settable"
  lua_settable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_setfield"
  lua_setfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawset"
  lua_rawset :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawseti"
  lua_rawseti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_setmetatable"
  lua_setmetatable :: LuaState -> CInt -> IO ()

#if LUA_VERSION_NUMBER < 502
foreign import ccall "lua.h lua_setfenv"
  lua_setfenv :: LuaState -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_setglobal"
  lua_setglobal :: LuaState -> Ptr CChar -> IO ()
#endif

--------------------------------------------------------------------------------
-- * 'load' and 'call' functions (load and run Lua code)

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_callk"
  lua_callk :: LuaState -> CInt -> CInt -> CInt -> Ptr () -> IO ()
#else
foreign import ccall "lua.h lua_call"
  lua_call :: LuaState -> CInt -> CInt -> IO ()
#endif

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_pcallk"
  lua_pcallk :: LuaState -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt
#else
foreign import ccall "lua.h lua_pcall"
  lua_pcall :: LuaState -> CInt -> CInt -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER < 502
foreign import ccall "lua.h lua_cpcall"
  lua_cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO CInt
#endif

foreign import ccall "lua.h lua_load"
  lua_load :: LuaState -> FunPtr LuaReader -> Ptr () -> Ptr CChar -> IO CInt

foreign import ccall "lua.h lua_dump"
  lua_dump :: LuaState -> FunPtr LuaWriter -> Ptr () -> IO ()


--------------------------------------------------------------------------------
-- * Coroutine functions

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_yieldk"
  lua_yieldk :: LuaState -> CInt -> CInt -> Ptr () -> IO CInt
#else
foreign import ccall "lua.h lua_yield"
  lua_yield :: LuaState -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lua.h lua_resume"
  lua_resume :: LuaState -> CInt -> CInt -> IO CInt
#else
foreign import ccall "lua.h lua_resume"
  lua_resume :: LuaState -> CInt -> IO CInt
#endif

foreign import ccall "lua.h lua_status"
  lua_status :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Garbage-collection functions and options

foreign import ccall "lua.h lua_gc"
  lua_gc :: LuaState -> CInt -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * Miscellaneous functions

foreign import ccall "lua.h lua_error"
  lua_error :: LuaState -> IO CInt

foreign import ccall "lua.h lua_next"
  lua_next :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_concat"
  lua_concat :: LuaState -> CInt -> IO ()

foreign import ccall "lualib.h luaL_openlibs"
  luaL_openlibs :: LuaState -> IO ()

foreign import ccall "lualib.h luaopen_base"
  lua_open_base :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_base"
  lua_open_base_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_table"
  lua_open_table :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_table"
  lua_open_table_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_io"
  lua_open_io :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_io"
  lua_open_io_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_os"
  lua_open_os :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_os"
  lua_open_os_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_string"
  lua_open_string :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_string"
  lua_open_string_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_math"
  lua_open_math :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_math"
  lua_open_math_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_debug"
  lua_open_debug :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_debug"
  lua_open_debug_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_package"
  lua_open_package :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_package"
  lua_open_package_ptr :: FunPtr (LuaState -> IO CInt)


--------------------------------------------------------------------------------
-- * The Auxiliary Library

foreign import ccall "lauxlib.h luaL_newstate"
  luaL_newstate :: IO LuaState

foreign import ccall "lauxlib.h luaL_newmetatable"
  luaL_newmetatable :: LuaState -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_argerror"
  luaL_argerror :: LuaState -> CInt -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_ref"
  luaL_ref :: LuaState -> CInt -> IO CInt

foreign import ccall "lauxlib.h luaL_unref"
  luaL_unref :: LuaState -> CInt -> CInt -> IO ()

#if LUA_VERSION_NUMBER >= 502
foreign import ccall "lauxlib.h luaL_loadfilex"
  luaL_loadfilex :: LuaState -> Ptr CChar -> Ptr CChar -> IO CInt
#else
foreign import ccall "lauxlib.h luaL_loadfile"
  luaL_loadfile :: LuaState -> Ptr CChar -> IO CInt
#endif

foreign import ccall "lauxlib.h luaL_loadstring"
  luaL_loadstring :: LuaState -> Ptr CChar -> IO CInt
