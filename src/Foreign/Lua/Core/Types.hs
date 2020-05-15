{-|
Module      : Foreign.Lua.Core.Types
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

The core Lua types, including mappings of Lua types to Haskell.

This module has mostly been moved to @'Foreign.Lua.Raw.Types'@ and
currently re-exports that module. This module might be removed in
the future.
-}
module Foreign.Lua.Core.Types
  ( Lua (..)
  , LuaEnvironment (..)
  , ErrorConversion (..)
  , errorConversion
  , State (..)
  , Reader
  , liftLua
  , liftLua1
  , state
  , runWithConverter
  , unsafeRunWith
  , unsafeErrorConversion
  , GCCONTROL (..)
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , liftIO
  , CFunction
  , LuaBool (..)
  , false
  , true
  , fromLuaBool
  , toLuaBool
  , Integer (..)
  , Number (..)
  , StackIndex (..)
  , nthFromBottom
  , nthFromTop
  , stackTop
  , stackBottom
  , NumArgs (..)
  , NumResults (..)
  , RelationalOperator (..)
  , fromRelationalOperator
  , Status (..)
  , StatusCode (..)
  , toStatus
    -- * References
  , Reference (..)
  , fromReference
  , toReference
  ) where

import Prelude hiding (Integer)

import Foreign.C (CInt)
import Foreign.Lua.Raw.Types

-- | Stack index of the nth element from the top of the stack.
nthFromTop :: CInt -> StackIndex
nthFromTop n = StackIndex (-n)
{-# INLINABLE nthFromTop #-}

-- | Stack index of the nth element from the bottom of the stack.
nthFromBottom :: CInt -> StackIndex
nthFromBottom = StackIndex
{-# INLINABLE nthFromBottom #-}

-- | Top of the stack
stackTop :: StackIndex
stackTop = -1
{-# INLINABLE stackTop #-}

-- | Bottom of the stack
stackBottom :: StackIndex
stackBottom = 1
{-# INLINABLE stackBottom #-}
