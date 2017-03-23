{-# LANGUAGE ForeignFunctionInterface #-}

module Scripting.Lua.Raw where

import Data.Int
import Foreign.C
import Foreign.Ptr

#include "lua.h"

-- TODO: lua_getallocf, lua_setallocf
-- TODO: Debugger functions

--------------------------------------------------------------------------------
-- * Types

-- | Synonym for @lua_State *@. See @lua_State@ in Lua Reference Manual.
newtype LuaState = LuaState (Ptr ())

-- | Synonym for @lua_Alloc@. See @lua_Alloc@ in Lua Reference Manual.
type LuaAlloc = Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ())

-- | Synonym for @lua_Reader@. See @lua_Reader@ in Lua Reference Manual.
type LuaReader = Ptr () -> Ptr () -> Ptr CSize -> IO (Ptr CChar)

-- | Synonym for @lua_Writer@. See @lua_Writer@ in Lua Reference Manual.
type LuaWriter = LuaState -> Ptr CChar -> CSize -> Ptr () -> IO CInt

-- | Synonym for @lua_CFunction@. See @lua_CFunction@ in Lua Reference Manual.
type LuaCFunction = LuaState -> IO CInt

-- | Synonym for @lua_Integer@. See @lua_Integer@ in Lua Reference Manual.
type LuaInteger = #{type LUA_INTEGER}

-- | Synonym for @lua_Number@. See @lua_Number@ in Lua Reference Manual.
type LuaNumber = #{type LUA_NUMBER}


-- Some of the Lua functions may call a Haskell function, and trigger
-- garbage collection, rescheduling etc. This means we must declare these
-- functions as 'safe'.
--
-- FIXME: Currently everthing marked as 'safe'. Change the ones that don't
-- call Haskell to 'unsafe'.


--------------------------------------------------------------------------------
-- * State manipulation

foreign import ccall "lua.h lua_newstate"
  c_lua_newstate :: FunPtr LuaAlloc -> Ptr () -> IO LuaState

foreign import ccall "lua.h lua_close"
  c_lua_close :: LuaState -> IO ()

foreign import ccall "lua.h lua_newthread"
  c_lua_newthread :: LuaState -> IO LuaState

foreign import ccall "lua.h lua_atpanic"
  c_lua_atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)


--------------------------------------------------------------------------------
-- * Basic stack manipulation

foreign import ccall "lua.h lua_gettop"
  c_lua_gettop :: LuaState -> IO CInt

foreign import ccall "lua.h lua_settop"
  c_lua_settop :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushvalue"
  c_lua_pushvalue :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rotate"
  c_lua_rotate :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_copy"
  c_lua_copy :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_checkstack"
  c_lua_checkstack :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_xmove"
  c_lua_xmove :: LuaState -> LuaState -> CInt -> IO ()


--------------------------------------------------------------------------------
-- * Stack access functions

foreign import ccall "lua.h lua_isnumber"
  c_lua_isnumber :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isstring"
  c_lua_isstring :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_iscfunction"
  c_lua_iscfunction :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_isuserdata"
  c_lua_isuserdata :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_type"
  c_lua_type :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_typename"
  c_lua_typename :: LuaState -> CInt -> IO (Ptr CChar)

foreign import ccall "lua.h lua_rawequal"
  c_lua_rawequal :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_tonumberx"
  c_lua_tonumberx :: LuaState -> CInt -> CInt -> IO LuaNumber

foreign import ccall "lua.h lua_tointegerx"
  c_lua_tointegerx :: LuaState -> CInt -> CInt -> IO LuaInteger

foreign import ccall "lua.h lua_toboolean"
  c_lua_toboolean :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_tolstring"
  c_lua_tolstring :: LuaState -> CInt -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall "lua.h lua_rawlen"
  c_lua_rawlen :: LuaState -> CInt -> IO CSize

foreign import ccall "lua.h lua_tocfunction"
  c_lua_tocfunction :: LuaState -> CInt -> IO (FunPtr LuaCFunction)

foreign import ccall "lua.h lua_touserdata"
  c_lua_touserdata :: LuaState -> CInt -> IO (Ptr a)

foreign import ccall "lua.h lua_tothread"
  c_lua_tothread :: LuaState -> CInt -> IO LuaState

foreign import ccall "lua.h lua_topointer"
  c_lua_topointer :: LuaState -> CInt -> IO (Ptr ())


--------------------------------------------------------------------------------
-- * Push functions

foreign import ccall "lua.h lua_pushnil"
  c_lua_pushnil :: LuaState -> IO ()

foreign import ccall "lua.h lua_pushnumber"
  c_lua_pushnumber :: LuaState -> LuaNumber -> IO ()

foreign import ccall "lua.h lua_pushinteger"
  c_lua_pushinteger :: LuaState -> LuaInteger -> IO ()

foreign import ccall "lua.h lua_pushlstring"
  c_lua_pushlstring :: LuaState -> Ptr CChar -> CSize -> IO ()

foreign import ccall "lua.h lua_pushstring"
  c_lua_pushstring :: LuaState -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_pushcclosure"
  c_lua_pushcclosure :: LuaState -> FunPtr LuaCFunction -> CInt -> IO ()

foreign import ccall "lua.h lua_pushboolean"
  c_lua_pushboolean :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_pushlightuserdata"
  c_lua_pushlightuserdata :: LuaState -> Ptr a -> IO ()

foreign import ccall "lua.h lua_pushthread"
  c_lua_pushthread :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Get functions

foreign import ccall "lua.h lua_getglobal"
  c_lua_getglobal :: LuaState -> Ptr CChar -> IO CInt

foreign import ccall "lua.h lua_gettable"
  c_lua_gettable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_getfield"
  c_lua_getfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawget"
  c_lua_rawget :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawgeti"
  c_lua_rawgeti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_createtable"
  c_lua_createtable :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_newuserdata"
  c_lua_newuserdata :: LuaState -> CInt -> IO (Ptr ())

foreign import ccall "lua.h lua_getmetatable"
  c_lua_getmetatable :: LuaState -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * Set functions

foreign import ccall "lua.h lua_settable"
  c_lua_settable :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_setglobal"
  c_lua_setglobal :: LuaState -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_setfield"
  c_lua_setfield :: LuaState -> CInt -> Ptr CChar -> IO ()

foreign import ccall "lua.h lua_rawset"
  c_lua_rawset :: LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_rawseti"
  c_lua_rawseti :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_setmetatable"
  c_lua_setmetatable :: LuaState -> CInt -> IO ()

--------------------------------------------------------------------------------
-- * 'load' and 'call' functions (load and run Lua code)

foreign import ccall "lua.h lua_callk"
  c_lua_callk :: LuaState -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "lua.h lua_pcallk"
  c_lua_pcallk :: LuaState -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_load"
  c_lua_load :: LuaState -> FunPtr LuaReader -> Ptr () -> Ptr CChar -> CInt -> IO CInt

foreign import ccall "lua.h lua_dump"
  c_lua_dump :: LuaState -> FunPtr LuaWriter -> Ptr () -> IO ()


--------------------------------------------------------------------------------
-- * Coroutine functions

foreign import ccall "lua.h lua_yieldk"
  c_lua_yieldk :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_resume"
  c_lua_resume :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_status"
  c_lua_status :: LuaState -> IO CInt


--------------------------------------------------------------------------------
-- * Garbage-collection functions and options

foreign import ccall "lua.h lua_gc"
  c_lua_gc :: LuaState -> CInt -> CInt -> IO CInt


--------------------------------------------------------------------------------
-- * Miscellaneous functions

foreign import ccall "lua.h lua_error"
  c_lua_error :: LuaState -> IO CInt

foreign import ccall "lua.h lua_next"
  c_lua_next :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_concat"
  c_lua_concat :: LuaState -> CInt -> IO ()

foreign import ccall "lualib.h luaL_openlibs"
  c_luaL_openlibs :: LuaState -> IO ()

foreign import ccall "lualib.h luaopen_base"
  c_lua_open_base :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_base"
  c_lua_open_base_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_table"
  c_lua_open_table :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_table"
  c_lua_open_table_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_io"
  c_lua_open_io :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_io"
  c_lua_open_io_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_os"
  c_lua_open_os :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_os"
  c_lua_open_os_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_string"
  c_lua_open_string :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_string"
  c_lua_open_string_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_math"
  c_lua_open_math :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_math"
  c_lua_open_math_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_debug"
  c_lua_open_debug :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_debug"
  c_lua_open_debug_ptr :: FunPtr (LuaState -> IO CInt)

foreign import ccall "lualib.h luaopen_package"
  c_lua_open_package :: LuaState -> IO CInt

foreign import ccall "lualib.h &luaopen_package"
  c_lua_open_package_ptr :: FunPtr (LuaState -> IO CInt)


--------------------------------------------------------------------------------
-- * The Auxiliary Library

foreign import ccall "lauxlib.h luaL_newstate"
  c_luaL_newstate :: IO LuaState

foreign import ccall "lauxlib.h luaL_newmetatable"
  c_luaL_newmetatable :: LuaState -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_argerror"
  c_luaL_argerror :: LuaState -> CInt -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_ref"
  c_luaL_ref :: LuaState -> CInt -> IO CInt

foreign import ccall "lauxlib.h luaL_unref"
  c_luaL_unref :: LuaState -> CInt -> CInt -> IO ()

foreign import ccall "lauxlib.h luaL_loadfilex"
  c_luaL_loadfilex :: LuaState -> Ptr CChar -> Ptr CChar -> IO CInt

foreign import ccall "lauxlib.h luaL_loadstring"
  c_luaL_loadstring :: LuaState -> Ptr CChar -> IO CInt
