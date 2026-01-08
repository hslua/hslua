{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Lua
Copyright   : © 2021-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : portable

Extend Haskell programs with a Lua interpreter.

This package provides the basic building blocks to integrate Lua into a
Haskell program. The library is kept very close to the C Lua API, and
users familiar with the C API should have no problem using it.

However, there are important differences of which users must be aware:
The method for error signaling used in Lua, based on @setjmp@ and
@longjmp@, is incompatible with the Haskell FFI. All errors /must/ be
handled at language boundaries, as failure to do so will lead to
unrecoverable crashes. C API functions that can throw Lua errors are
still exported, but non-error throwing @hslua_@ versions are provided as
safer alternatives. . The @hslua@ ersatz functions have worse
performance than the original versions, but should be fast enough for
most use cases.

The Haskell FFI requires all C function that can call back into Haskell
to be imported @safe@ly. Some of the Lua functions may, directly or
indirectly, call a Haskell function, so they are always imported with
the @safe@ keyword.

Many API functions can trigger garbage collection. This will lead to
problems if Haskell functions are used as part of finalizers (i.e.,
@__gc@ metamethods). Haskell in finalizers is not supported by default,
but can be enabled by unsetting the @allow-unsafe-gc@ flag.
-}
module Lua
  ( -- * Run Lua operations
    withNewState
    -- * Types
  , State (..)
  , Reader
    -- ** Base Lua types
  , CFunction
  , PreCFunction
  , Lua.Integer (..)
  , Number (..)
    -- *** Booleans
  , LuaBool (..)
  , pattern TRUE
  , pattern FALSE
    -- ** Stack indices
  , StackIndex (..)
  , pattern LUA_REGISTRYINDEX
    -- ** Function calling
  , NumArgs (..)
  , NumResults (..)
  , pattern LUA_MULTRET
    -- ** Basic types
  , TypeCode (..)
  , pattern LUA_TNONE
  , pattern LUA_TNIL
  , pattern LUA_TBOOLEAN
  , pattern LUA_TLIGHTUSERDATA
  , pattern LUA_TNUMBER
  , pattern LUA_TSTRING
  , pattern LUA_TTABLE
  , pattern LUA_TFUNCTION
  , pattern LUA_TUSERDATA
  , pattern LUA_TTHREAD
    -- ** Relational operator codes
  , OPCode (..)
  , pattern LUA_OPEQ
  , pattern LUA_OPLT
  , pattern LUA_OPLE
    -- ** Codes for arithmetic operations
  , pattern LUA_OPADD
  , pattern LUA_OPSUB
  , pattern LUA_OPMUL
  , pattern LUA_OPDIV
  , pattern LUA_OPIDIV
  , pattern LUA_OPMOD
  , pattern LUA_OPPOW
  , pattern LUA_OPUNM
  , pattern LUA_OPBNOT
  , pattern LUA_OPBAND
  , pattern LUA_OPBOR
  , pattern LUA_OPBXOR
  , pattern LUA_OPSHL
  , pattern LUA_OPSHR
    -- ** Status codes
  , StatusCode (..)
  , pattern LUA_OK
  , pattern LUA_YIELD
  , pattern LUA_ERRRUN
  , pattern LUA_ERRSYNTAX
  , pattern LUA_ERRMEM
  , pattern LUA_ERRERR
  , pattern LUA_ERRFILE

    -- * Stack index helpers
  , nthTop
  , nthBottom
  , nth
  , top
    -- * Functions
  , -- ** State manipulation
    lua_close
  , lua_newthread
  , lua_version
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
  , lua_rotate
  , lua_checkstack
    -- ** Access functions (stack → Haskell)
  , lua_isnil
  , lua_isboolean
  , lua_isnumber
  , lua_isinteger
  , lua_isstring
  , lua_isfunction
  , lua_istable
  , lua_iscfunction
  , lua_isuserdata
  , lua_islightuserdata
  , lua_isthread
  , lua_isnone
  , lua_isnoneornil
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
  , lua_pushstring
  , lua_pushcclosure
  , lua_pushcfunction
  , lua_pushboolean
  , lua_pushlightuserdata
  , lua_pushthread
    -- ** Get functions (Lua → stack)
  , lua_rawget
  , lua_rawgeti
  , lua_createtable
  , lua_newuserdatauv
  , lua_getmetatable
  , lua_getiuservalue
  , lua_getglobal
  , lua_gettable
    -- ** Set functions (stack → Lua)
  , lua_rawset
  , lua_rawseti
  , lua_setmetatable
  , lua_setiuservalue
  , lua_setglobal
  , lua_settable
    -- ** Misc (safe)
  , lua_stringtonumber
    -- ** Misc (unsafe)
  , lua_arith
  , lua_concat
  , lua_next
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
  , pattern LUA_GCGEN
  , pattern LUA_GCINC
    -- ** Warning-related functions
  , lua_warning
  , lua_setwarnf
  , WarnFunction
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
  , pattern LUA_REFNIL
  , pattern LUA_NOREF
  , fromReference
  , toReference

    -- * Debug interface
  , lua_getupvalue
  , lua_setupvalue

    -- * Ersatz functions
  , hsluaL_newstate
  , hsluaL_tolstring
  , hsluaL_requiref
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
  , hslua_arith
  , hslua_compare
    -- ** Simplified warnings handling
  , hsluaL_setwarnf

    -- * Standard Lua libraries
  , luaopen_base
  , luaopen_table
  , luaopen_io
  , luaopen_os
  , luaopen_string
  , luaopen_math
  , luaopen_debug
  , luaopen_package

    -- * Push Haskell functions
  , hslua_pushhsfunction

    -- * Predefined values in the registry
  , pattern LUA_RIDX_GLOBALS

    -- * Version and copyright info
  , pattern LUA_VERSION
  , pattern LUA_RELEASE
  , pattern LUA_COPYRIGHT
  ) where

import Foreign.C (CInt)
import Lua.Auxiliary
import Lua.Call
import Lua.Constants
import Lua.Debug
import Lua.Ersatz.Functions
import Lua.Ersatz.Auxiliary
import Lua.Lib
import Lua.Primary
import Lua.Types as Lua
import Lua.Warn

-- | Runs operations on a new Lua @'Lua.State'@. The state is created
-- when the function is called and closed on return. The state, and all
-- pointers to values within it, __must not__ be used after the function
-- returns.
--
-- === Example
-- Run a small Lua operation (prints the major version of Lua).
--
-- > withNewState $ \l -> do
-- >   luaL_openlibs l
-- >   withCString "print" (lua_getglobal l)
-- >   withCString "_VERSION" (lua_getglobal l)
-- >   lua_pcall l (NumArgs 1) (NumResults 1) (StackIndex 0)
--
-- @since 2.0.0
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
--
-- @since 2.0.0
nthTop :: CInt -> StackIndex
nthTop n = StackIndex (-n)
{-# INLINABLE nthTop #-}

-- | Stack index of the nth element from the bottom of the stack.
--
-- @since 2.0.0
nthBottom :: CInt -> StackIndex
nthBottom = StackIndex
{-# INLINABLE nthBottom #-}

-- | Alias for 'nthTop'.
--
-- @since 2.0.0
nth :: CInt -> StackIndex
nth = nthTop
{-# INLINABLE nth #-}

-- | Index of the topmost stack element.
--
-- @since 2.0.0
top :: StackIndex
top = nthTop 1
{-# INLINABLE top #-}
