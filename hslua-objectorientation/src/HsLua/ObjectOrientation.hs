{-# LANGUAGE ScopedTypeVariables   #-}
{-|
Module      : HsLua.ObjectOrientation
Copyright   : © 2021-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

This module provides types and functions to use Haskell values as
userdata objects in Lua. These objects wrap a Haskell value and provide
methods and properties to interact with the Haskell value.

The terminology in this module refers to the userdata values as /UD
objects/, and to their type as /UD type/.
-}
module HsLua.ObjectOrientation
  ( UDType
  , deftypeGeneric
  , module HsLua.ObjectOrientation.Generic
  , module HsLua.ObjectOrientation.Operation
  ) where

import Data.Void (Void)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation.Generic
import HsLua.ObjectOrientation.Operation

-- | A userdata type, capturing the behavior of Lua objects that wrap
-- Haskell values. The type name must be unique; once the type has been
-- used to push or retrieve a value, the behavior can no longer be
-- modified through this type.
type UDType e fn a = UDTypeWithList e fn a Void

-- | Defines a new type, defining the behavior of objects in Lua.
-- Note that the type name must be unique.
deftypeGeneric :: Pusher e fn           -- ^ function pusher
               -> Name                  -- ^ type name
               -> [(Operation, fn)]     -- ^ operations
               -> [Member e fn a]       -- ^ methods
               -> UDType e fn a
deftypeGeneric pushFunction name ops members =
  deftypeGeneric' pushFunction name ops members Nothing
