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
import Foreign.Lua.Api.Types
import Foreign.Ptr

#include "lua.h"

-- TODO: lua_getallocf, lua_setallocf
-- TODO: Debugger functions

-- Some of the Lua functions may call a Haskell function, and trigger
-- garbage collection, rescheduling etc. This means we must declare these
-- functions as 'safe'.


--------------------------------------------------------------------------------
-- * State manipulation

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newstate lua_newstate>
foreign import ccall unsafe "lua.h lua_newstate"
  lua_newstate :: FunPtr LuaAlloc -> Ptr () -> IO LuaState

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_close lua_close>
foreign import ccall "lua.h lua_close"
  lua_close :: LuaState -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newthread lua_newthread>
foreign import ccall unsafe "lua.h lua_newthread"
  lua_newthread :: LuaState -> IO LuaState

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_atpanic lua_atpanic>
foreign import ccall "lua.h lua_atpanic"
  lua_atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)


--------------------------------------------------------------------------------
-- * Basic stack manipulation

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gettop lua_gettop>
foreign import ccall unsafe "lua.h lua_gettop"
  lua_gettop :: LuaState -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_settop lua_settop>
foreign import ccall unsafe "lua.h lua_settop"
  lua_settop :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushvalue lua_pushvalue>
foreign import ccall unsafe "lua.h lua_pushvalue"
  lua_pushvalue :: LuaState -> CInt -> IO ()

#if LUA_VERSION_NUMBER >= 503
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rotate lua_rotate>
foreign import ccall unsafe "lua.h lua_rotate"
  lua_rotate :: LuaState -> CInt -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_copy lua_copy>
foreign import ccall unsafe "lua.h lua_copy"
  lua_copy :: LuaState -> CInt -> CInt -> IO ()
#else
-- | See <https://www.lua.org/manual/5.2/manual.html#lua_remove lua_remove>
foreign import ccall unsafe "lua.h lua_remove"
  lua_remove :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_insert lua_insert>
foreign import ccall unsafe "lua.h lua_insert"
  lua_insert :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_replace lua_replace>
foreign import ccall unsafe "lua.h lua_replace"
  lua_replace :: LuaState -> CInt -> IO ()
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_checkstack lua_checkstack>
foreign import ccall unsafe "lua.h lua_checkstack"
  lua_checkstack :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_xmove lua_xmove>
foreign import ccall unsafe "lua.h lua_xmove"
  lua_xmove :: LuaState -> LuaState -> CInt -> IO ()


--------------------------------------------------------------------------------
-- * Stack access functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnumber lua_isnumber>
foreign import ccall unsafe "lua.h lua_isnumber"
  lua_isnumber :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isstring lua_isstring>
foreign import ccall unsafe "lua.h lua_isstring"
  lua_isstring :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_iscfunction lua_iscfunction>
foreign import ccall unsafe "lua.h lua_iscfunction"
  lua_iscfunction :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isuserdata lua_isuserdata>
foreign import ccall unsafe "lua.h lua_isuserdata"
  lua_isuserdata :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>
foreign import ccall unsafe "lua.h lua_type"
  lua_type :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_typename lua_typename>
foreign import ccall unsafe "lua.h lua_typename"
  lua_typename :: LuaState -> CInt -> IO (Ptr CChar)

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_compare lua_compare>
foreign import ccall "lua.h lua_compare"
  lua_compare :: LuaState -> CInt -> CInt -> CInt -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_equal lua_equal>
foreign import ccall "lua.h lua_equal"
  lua_equal :: LuaState -> CInt -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_lessthan lua_lessthan>
foreign import ccall "lua.h lua_lessthan"
  lua_lessthan :: LuaState -> CInt -> CInt -> IO CInt
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawequal lua_rawequal>
foreign import ccall unsafe "lua.h lua_rawequal"
  lua_rawequal :: LuaState -> CInt -> CInt -> IO CInt

--
-- Type coercion
--
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_toboolean lua_toboolean>
foreign import ccall unsafe "lua.h lua_toboolean"
  lua_toboolean :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction lua_tocfunction>
foreign import ccall unsafe "lua.h lua_tocfunction"
  lua_tocfunction :: LuaState -> CInt -> IO (FunPtr LuaCFunction)

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tointegerx lua_tointegerx>
foreign import ccall unsafe "lua.h lua_tointegerx"
  lua_tointegerx :: LuaState -> CInt -> CInt -> IO LuaInteger

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tonumberx lua_tonumberx>
foreign import ccall unsafe "lua.h lua_tonumberx"
  lua_tonumberx :: LuaState -> CInt -> CInt -> IO LuaNumber
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tointeger lua_tointeger>
foreign import ccall unsafe "lua.h lua_tointeger"
  lua_tointeger :: LuaState -> CInt -> IO LuaInteger

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tonumber lua_tonumber>
foreign import ccall unsafe "lua.h lua_tonumber"
  lua_tonumber :: LuaState -> CInt -> IO LuaNumber
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tolstring lua_tolstring>
foreign import ccall unsafe "lua.h lua_tolstring"
  lua_tolstring :: LuaState -> CInt -> Ptr CSize -> IO (Ptr CChar)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_topointer lua_topointer>
foreign import ccall unsafe "lua.h lua_topointer"
  lua_topointer :: LuaState -> CInt -> IO (Ptr ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tothread lua_tothread>
foreign import ccall unsafe "lua.h lua_tothread"
  lua_tothread :: LuaState -> CInt -> IO LuaState

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_touserdata lua_touserdata>
foreign import ccall unsafe "lua.h lua_touserdata"
  lua_touserdata :: LuaState -> CInt -> IO (Ptr a)


--
-- Object size
--

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawlen lua_rawlen>
foreign import ccall unsafe "lua.h lua_rawlen"
  lua_rawlen :: LuaState -> CInt -> IO CSize
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_objlen lua_objlen>
foreign import ccall unsafe "lua.h lua_objlen"
  lua_objlen :: LuaState -> CInt -> IO CSize
#endif


--------------------------------------------------------------------------------
-- * Push functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnil lua_pushnil>
foreign import ccall unsafe "lua.h lua_pushnil"
  lua_pushnil :: LuaState -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnumber lua_pushnumber>
foreign import ccall unsafe "lua.h lua_pushnumber"
  lua_pushnumber :: LuaState -> LuaNumber -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushinteger lua_pushinteger>
foreign import ccall unsafe "lua.h lua_pushinteger"
  lua_pushinteger :: LuaState -> LuaInteger -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushlstring lua_pushlstring>
foreign import ccall unsafe "lua.h lua_pushlstring"
  lua_pushlstring :: LuaState -> Ptr CChar -> CSize -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushstring lua_pushstring>
foreign import ccall unsafe "lua.h lua_pushstring"
  lua_pushstring :: LuaState -> Ptr CChar -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushcclosure lua_pushcclosure>
foreign import ccall unsafe "lua.h lua_pushcclosure"
  lua_pushcclosure :: LuaState -> FunPtr LuaCFunction -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushboolean lua_pushboolean>
foreign import ccall unsafe "lua.h lua_pushboolean"
  lua_pushboolean :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushlightuserdata lua_pushlightuserdata>
foreign import ccall unsafe "lua.h lua_pushlightuserdata"
  lua_pushlightuserdata :: LuaState -> Ptr a -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushthread lua_pushthread>
foreign import ccall unsafe "lua.h lua_pushthread"
  lua_pushthread :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Get functions

#if LUA_VERSION_NUMBER >= 503
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>
foreign import ccall "lua.h lua_gettable"
  lua_gettable :: LuaState -> CInt -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.2/manual.html#lua_gettable lua_gettable>
foreign import ccall "lua.h lua_gettable"
  lua_gettable :: LuaState -> CInt -> IO ()
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getfield lua_getfield>
foreign import ccall "lua.h lua_getfield"
  lua_getfield :: LuaState -> CInt -> Ptr CChar -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawget lua_rawget>
foreign import ccall unsafe "lua.h lua_rawget"
  lua_rawget :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti lua_rawgeti>
foreign import ccall unsafe "lua.h lua_rawgeti"
  lua_rawgeti :: LuaState -> CInt -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_createtable lua_createtable>
foreign import ccall unsafe "lua.h lua_createtable"
  lua_createtable :: LuaState -> CInt -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newuserdata lua_newuserdata>
foreign import ccall unsafe "lua.h lua_newuserdata"
  lua_newuserdata :: LuaState -> CInt -> IO (Ptr ())

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getmetatable lua_getmetatable>
foreign import ccall unsafe "lua.h lua_getmetatable"
  lua_getmetatable :: LuaState -> CInt -> IO CInt

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_getfenv lua_getfenv>
foreign import ccall "lua.h lua_getfenv"
  lua_getfenv :: LuaState -> CInt -> IO ()
#endif

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>
foreign import ccall "lua.h lua_getglobal"
  lua_getglobal :: LuaState -> Ptr CChar -> IO ()
#endif

--------------------------------------------------------------------------------
-- * Set functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_settable lua_settable>
foreign import ccall "lua.h lua_settable"
  lua_settable :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setfield lua_setfield>
foreign import ccall "lua.h lua_setfield"
  lua_setfield :: LuaState -> CInt -> Ptr CChar -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawset lua_rawset>
foreign import ccall unsafe "lua.h lua_rawset"
  lua_rawset :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawseti lua_rawseti>
foreign import ccall unsafe "lua.h lua_rawseti"
  lua_rawseti :: LuaState -> CInt -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setmetatable lua_setmetatable>
foreign import ccall unsafe "lua.h lua_setmetatable"
  lua_setmetatable :: LuaState -> CInt -> IO ()

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_setfenv lua_setfenv>
foreign import ccall "lua.h lua_setfenv"
  lua_setfenv :: LuaState -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setglobal lua_setglobal>
foreign import ccall "lua.h lua_setglobal"
  lua_setglobal :: LuaState -> Ptr CChar -> IO ()
#endif

--------------------------------------------------------------------------------
-- * 'load' and 'call' functions (load and run Lua code)

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_callk lua_callk>
foreign import ccall "lua.h lua_callk"
  lua_callk :: LuaState -> CInt -> CInt -> CInt -> Ptr () -> IO ()
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_call lua_call>
foreign import ccall "lua.h lua_call"
  lua_call :: LuaState -> CInt -> CInt -> IO ()
#endif

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pcallk lua_pcallk>
foreign import ccall "lua.h lua_pcallk"
  lua_pcallk :: LuaState -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pcall lua_pcall>
foreign import ccall "lua.h lua_pcall"
  lua_pcall :: LuaState -> CInt -> CInt -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_cpcall lua_cpcall>
foreign import ccall "lua.h lua_cpcall"
  lua_cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO CInt
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_load lua_load>
foreign import ccall "lua.h lua_load"
  lua_load :: LuaState -> FunPtr LuaReader -> Ptr () -> Ptr CChar -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_dump lua_dump>
foreign import ccall "lua.h lua_dump"
  lua_dump :: LuaState -> FunPtr LuaWriter -> Ptr () -> IO ()


--------------------------------------------------------------------------------
-- * Coroutine functions

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_yieldk lua_yieldk>
foreign import ccall "lua.h lua_yieldk"
  lua_yieldk :: LuaState -> CInt -> CInt -> Ptr () -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_yield lua_yield>
foreign import ccall "lua.h lua_yield"
  lua_yield :: LuaState -> CInt -> IO CInt
#endif

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#lua_resume lua_resume>
foreign import ccall "lua.h lua_resume"
  lua_resume :: LuaState -> CInt -> CInt -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#lua_resume lua_resume>
foreign import ccall "lua.h lua_resume"
  lua_resume :: LuaState -> CInt -> IO CInt
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_status lua_status>
foreign import ccall unsafe "lua.h lua_status"
  lua_status :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Garbage-collection functions and options

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gc lua_gc>
foreign import ccall "lua.h lua_gc"
  lua_gc :: LuaState -> CInt -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * Miscellaneous functions

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_error lua_error>
foreign import ccall unsafe "lua.h lua_error"
  lua_error :: LuaState -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_next lua_next>
foreign import ccall "lua.h lua_next"
  lua_next :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_concat lua_concat>
foreign import ccall "lua.h lua_concat"
  lua_concat :: LuaState -> CInt -> IO ()

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_openlibs luaL_openlibs>
foreign import ccall unsafe "lualib.h luaL_openlibs"
  luaL_openlibs :: LuaState -> IO ()

-- | Opens the base library.
foreign import ccall unsafe "lualib.h luaopen_base"
  lua_open_base :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_base'@
foreign import ccall unsafe "lualib.h &luaopen_base"
  lua_open_base_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the table library.
foreign import ccall unsafe "lualib.h luaopen_table"
  lua_open_table :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_table'@
foreign import ccall unsafe "lualib.h &luaopen_table"
  lua_open_table_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the io library.
foreign import ccall unsafe "lualib.h luaopen_io"
  lua_open_io :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_io'@
foreign import ccall unsafe "lualib.h &luaopen_io"
  lua_open_io_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the os library.
foreign import ccall unsafe "lualib.h luaopen_os"
  lua_open_os :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_os'@
foreign import ccall unsafe "lualib.h &luaopen_os"
  lua_open_os_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the string library.
foreign import ccall unsafe "lualib.h luaopen_string"
  lua_open_string :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_string'@
foreign import ccall unsafe "lualib.h &luaopen_string"
  lua_open_string_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the math library.
foreign import ccall unsafe "lualib.h luaopen_math"
  lua_open_math :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_math'@
foreign import ccall unsafe "lualib.h &luaopen_math"
  lua_open_math_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the debug library.
foreign import ccall unsafe "lualib.h luaopen_debug"
  lua_open_debug :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_debug'@
foreign import ccall unsafe "lualib.h &luaopen_debug"
  lua_open_debug_ptr :: FunPtr (LuaState -> IO CInt)

-- | Opens the package library.
foreign import ccall unsafe "lualib.h luaopen_package"
  lua_open_package :: LuaState -> IO CInt

-- | Function pointer to @'lua_open_package'@
foreign import ccall unsafe "lualib.h &luaopen_package"
  lua_open_package_ptr :: FunPtr (LuaState -> IO CInt)


--------------------------------------------------------------------------------
-- * The Auxiliary Library

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>
foreign import ccall unsafe "lauxlib.h luaL_newstate"
  luaL_newstate :: IO LuaState

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_newmetatable luaL_newmetatable>
foreign import ccall "lauxlib.h luaL_newmetatable"
  luaL_newmetatable :: LuaState -> Ptr CChar -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_argerror luaL_argerror>
foreign import ccall "lauxlib.h luaL_argerror"
  luaL_argerror :: LuaState -> CInt -> Ptr CChar -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_ref luaL_ref>
foreign import ccall "lauxlib.h luaL_ref"
  luaL_ref :: LuaState -> CInt -> IO CInt

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>
foreign import ccall "lauxlib.h luaL_unref"
  luaL_unref :: LuaState -> CInt -> CInt -> IO ()

#if LUA_VERSION_NUMBER >= 502
-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfilex luaL_loadfilex>
foreign import ccall "lauxlib.h luaL_loadfilex"
  luaL_loadfilex :: LuaState -> Ptr CChar -> Ptr CChar -> IO CInt
#else
-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_loadfile luaL_loadfile>
foreign import ccall "lauxlib.h luaL_loadfile"
  luaL_loadfile :: LuaState -> Ptr CChar -> IO CInt
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>
foreign import ccall unsafe "lauxlib.h luaL_loadstring"
  luaL_loadstring :: LuaState -> Ptr CChar -> IO CInt
