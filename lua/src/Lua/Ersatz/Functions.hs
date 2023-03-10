{-# LANGUAGE CPP #-}
{-|
Module      : Lua.Ersatz.Functions
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : ForeignFunctionInterface, CPP

Ersatz functions for Lua API items which may, directly or indirectly,
throw a Lua error.
-}
module Lua.Ersatz.Functions
  ( hslua_arith
  , hslua_compare
    -- * Get functions (Lua -> stack)
  , hslua_gettable
  , hslua_getglobal
    -- * Set functions (stack -> Lua)
  , hslua_settable
  , hslua_setglobal
    -- * Misc
  , hslua_error
  , hslua_next
  , hslua_concat
  )where

import Foreign.C
import Lua.Types as Lua
import Foreign.Ptr

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- | Performs an arithmetic or bitwise operation over the two values (or
-- one, in the case of negations) at the top of the stack, with the
-- value at the top being the second operand, pops these values, and
-- pushes the result of the operation. The function follows the
-- semantics of the corresponding Lua operator (that is, it may call
-- metamethods).
--
-- The value of @op@ must be one of the following constants:
--
-- -   __@LUA_OPADD@:__ performs addition (@+@)
-- -   __@LUA_OPSUB@:__ performs subtraction (@-@)
-- -   __@LUA_OPMUL@:__ performs multiplication (@*@)
-- -   __@LUA_OPDIV@:__ performs float division (@\/@)
-- -   __@LUA_OPIDIV@:__ performs floor division (@\/\/@)
-- -   __@LUA_OPMOD@:__ performs modulo (@%@)
-- -   __@LUA_OPPOW@:__ performs exponentiation (@^@)
-- -   __@LUA_OPUNM@:__ performs mathematical negation (unary @-@)
-- -   __@LUA_OPBNOT@:__ performs bitwise NOT (@~@)
-- -   __@LUA_OPBAND@:__ performs bitwise AND (@&@)
-- -   __@LUA_OPBOR@:__ performs bitwise OR (@|@)
-- -   __@LUA_OPBXOR@:__ performs bitwise exclusive OR (@~@)
-- -   __@LUA_OPSHL@:__ performs left shift (@\<\<@)
-- -   __@LUA_OPSHR@:__ performs right shift (@>>@)
--
-- This function wraps @lua_arith@ and takes an additional parameter
-- @status@; if it is not @NULL@, then the return value is set to the
-- status after calling @lua_arith@.
foreign import ccall SAFTY "hslua.h hslua_arith"
  hslua_arith :: State
              -> ArithOPCode     -- ^ op
              -> Ptr StatusCode
              -> IO ()

-- | Compares two Lua values. Returns @1@ if the value at index @index1@
-- satisfies op when compared with the value at index @index2@,
-- following the semantics of the corresponding Lua operator (that is,
-- it may call metamethods). Otherwise returns @0@. Also returns @0@ if
-- any of the indices is not valid.
--
-- The value of op must be one of the following constants:
--
--  - 'Lua.Constants.LUA_OPEQ': compares for equality (==)
--  - 'Lua.Constants.LUA_OPLT': compares for less than (<)
--  - 'Lua.Constants.LUA_OPLE': compares for less or equal (<=)
--
-- This function wraps @lua_compare@ and takes an additional parameter
-- @status@; if it is not @NULL@, then the return value is set to the
-- status after calling @lua_compare@.
foreign import capi safe "hslua.h hslua_compare"
  hslua_compare :: Lua.State
                -> StackIndex     -- ^ index 1
                -> StackIndex     -- ^ index 2
                -> OPCode         -- ^ operator
                -> Ptr StatusCode -- ^ status
                -> IO LuaBool

--
-- Get functions (Lua -> stack)
--

-- | Behaves like @'Lua.Primary.lua_gettable'@, but prevents
-- unrecoverable program crashes by calling that function through
-- @'Lua.lua_pcall'@. Takes an additional status code pointer
-- that is set to the status returned by @lua_pcall@.
foreign import ccall safe "hslua.h hslua_gettable"
  hslua_gettable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO TypeCode

-- | Behaves like @'Lua.Primary.lua_getglobal'@, but prevents
-- unrecoverable program crashes by calling that function through
-- @'Lua.lua_pcall'@. Takes an additional status code pointer
-- that is set to the status returned by @lua_pcall@.
foreign import ccall safe "hslua.h hslua_getglobal"
  hslua_getglobal :: Lua.State
                  -> CString
                  -> CSize
                  -> Ptr StatusCode
                  -> IO TypeCode

--
-- Set functions (stack -> Lua)
--

-- | Behaves like @'Lua.Primary.lua_settable'@, but prevents
-- unrecoverable program crashes by calling that function through
-- @'Lua.lua_pcall'@. Takes an additional status code pointer
-- that is set to the status returned by @lua_pcall@.
foreign import ccall safe "hslua.h hslua_settable"
  hslua_settable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO ()

-- | Behaves like @'Lua.Primary.lua_setglobal'@, but prevents
-- unrecoverable program crashes by calling that function through
-- @'Lua.lua_pcall'@. Takes an additional status code pointer
-- that is set to the status returned by @lua_pcall@.
foreign import ccall safe "hslua.h hslua_setglobal"
  hslua_setglobal :: Lua.State -> CString -> CSize -> Ptr StatusCode -> IO ()

--
-- Miscellaneous functions
--

-- | Replacement for
-- <https://lua.org/manual/5.4/manual.html#lua_error lua_error>; it uses
-- the HsLua error signaling convention instead of raw Lua errors.
foreign import ccall SAFTY "hslua.h hslua_error"
  hslua_error :: Lua.State -> IO NumResults

-- | Wrapper around
-- <https://lua.org/manual/5.4/manual.html#lua_next lua_next> which
-- catches any Lua errors.
foreign import ccall safe "hslua.h hslua_next"
  hslua_next :: Lua.State -> StackIndex -> Ptr StatusCode -> IO LuaBool

-- | Wrapper around
-- <https://lua.org/manual/5.4/manual.html#lua_concat lua_concat>
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_concat"
  hslua_concat :: Lua.State -> NumArgs -> Ptr StatusCode -> IO ()
