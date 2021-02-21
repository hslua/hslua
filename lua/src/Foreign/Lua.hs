{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Foreign.Lua
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

Extend Haskell programs with a Lua interpreter.
-}
module Foreign.Lua
  ( withNewState
    -- * Types
  , State (..)
  , Reader
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , CFunction
  , LuaBool (..)
  , false
  , true
  , fromLuaBool
  , toLuaBool
  , Lua.Integer (..)
  , Number (..)
  , StackIndex (..)
  , NumArgs (..)
  , NumResults (..)
  , RelationalOperator (..)
  , fromRelationalOperator
  , StatusCode (..)
    -- * Status codes
  , pattern LUA_OK
  , pattern LUA_YIELD
  , pattern LUA_ERRRUN
  , pattern LUA_ERRSYNTAX
  , pattern LUA_ERRMEM
  , pattern LUA_ERRGCMM
  , pattern LUA_ERRERR
  , pattern LUA_ERRFILE

    -- * Constants
  , multret
  , registryindex
  , refnil
  , noref

    -- * Stack index helpers
  , nthTop
  , nthBottom
  , nth
  , top
    -- * Functions
  , -- ** State manipulation
    lua_close
  , lua_newthread
    -- ** Basic stack manipulation
  , lua_absindex
  , lua_gettop
  , lua_settop
  , lua_pushvalue
  , lua_pop
  , lua_copy
  , lua_remove
  , lua_insert
  , lua_replace
  , lua_checkstack
    -- ** Access functions (stack → Haskell)
  , lua_isnumber
  , lua_isinteger
  , lua_isstring
  , lua_iscfunction
  , lua_isuserdata
  , lua_type
  , lua_typename
  , lua_rawequal
  , lua_toboolean
  , lua_tocfunction
  , lua_tointegerx
  , lua_tonumberx
  , lua_tolstring
  , lua_topointer
  , lua_tothread
  , lua_touserdata
  , lua_rawlen
    -- ** Push functions (Haskell → stack)
  , lua_pushnil
  , lua_pushnumber
  , lua_pushinteger
  , lua_pushlstring
  , lua_pushcclosure
  , lua_pushboolean
  , lua_pushlightuserdata
  , lua_pushthread
    -- ** Get functions (Lua → stack)
  , lua_rawget
  , lua_rawgeti
  , lua_createtable
  , lua_newuserdata
  , lua_getmetatable
    -- ** Set functions (stack → Lua)
  , lua_rawset
  , lua_rawseti
  , lua_setmetatable
    -- ** Load and run Lua code
  , lua_pcall
  , lua_load
    -- ** Coroutine functions
  , lua_status
    -- ** Garbage-collection
  , lua_gc
  , GCCode (..)
  , pattern LUA_GCSTOP
  , pattern LUA_GCRESTART
  , pattern LUA_GCCOLLECT
  , pattern LUA_GCCOUNT
  , pattern LUA_GCCOUNTB
  , pattern LUA_GCSTEP
  , pattern LUA_GCSETPAUSE
  , pattern LUA_GCSETSTEPMUL
  , pattern LUA_GCISRUNNING
    -- ** Miscellaneous functions
  , lua_pushglobaltable

    -- * The Auxiliary Library
  , luaL_getmetafield
  , luaL_getmetatable
  , luaL_loadbuffer
  , luaL_openlibs
  , luaL_newmetatable
  , luaL_ref
  , luaL_testudata
  , luaL_traceback
  , luaL_unref
    -- ** Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
    -- ** References
  , Reference (..)
  , fromReference
  , toReference

    -- * Ersatz functions
  , hsluaL_newstate
  , hsluaL_tolstring
  , hslua_compare
    -- ** Get functions (Lua → stack)
  , hslua_gettable
  , hslua_getglobal
    -- ** Set functions (stack → Lua)
  , hslua_settable
  , hslua_setglobal
    -- ** Misc
  , hslua_error
  , hslua_next
  , hslua_concat

    -- * Standard Lua libraries
  , luaopen_base
  , luaopen_table
  , luaopen_io
  , luaopen_os
  , luaopen_string
  , luaopen_math
  , luaopen_debug
  , luaopen_package
  ) where

import Foreign.C (CInt)
import Foreign.Lua.Auxiliary
import Foreign.Lua.Constants
import Foreign.Lua.Ersatz.Functions
import Foreign.Lua.Ersatz.Auxiliary
import Foreign.Lua.Functions
import Foreign.Lua.Lib
import Foreign.Lua.Types as Lua

-- | Runs operations on a new Lua @'Lua.State'@. The state is created
-- when the function is called and closed on return. The state, and all
-- pointers to values within it, __must not__ be used after the function
-- returns.
withNewState :: (State -> IO a) -> IO a
withNewState f = do
  l <- hsluaL_newstate
  result <- f l
  lua_close l
  return result

--
-- Stack index helpers
--

-- | Stack index of the nth element from the top of the stack.
nthTop :: CInt -> StackIndex
nthTop n = StackIndex (-n)
{-# INLINABLE nthTop #-}

-- | Stack index of the nth element from the bottom of the stack.
nthBottom :: CInt -> StackIndex
nthBottom = StackIndex
{-# INLINABLE nthBottom #-}

-- | Alias for 'nthTop'.
nth :: CInt -> StackIndex
nth = nthTop
{-# INLINABLE nth #-}

-- | Index of the topmost stack element.
top :: StackIndex
top = nthTop 1
{-# INLINABLE top #-}
