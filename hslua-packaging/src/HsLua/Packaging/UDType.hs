{-|
Module      : HsLua.Packaging.UDType
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

This module provides types and functions to use Haskell values as
userdata objects in Lua. These objects wrap a Haskell value and provide
methods and properties to interact with the Haskell value.

The terminology in this module refers to the userdata values as /UD
objects/, and to their type as /UD type/.
-}
module HsLua.Packaging.UDType
  ( DocumentedType
  , DocumentedTypeWithList
  , deftype
  , deftype'
  , method
  , property
  , possibleProperty
  , readonly
  , alias
  , operation
  , peekUD
  , pushUD
  , udparam
  , udresult
    -- * Helper types for building
  , Member
  , Operation (..)
  , Property
  , Possible (..)
  ) where

import Data.Text (Text)
import HsLua.Core
import HsLua.ObjectOrientation
import HsLua.ObjectOrientation.Operation (metamethodName)
import HsLua.Packaging.Function
import qualified HsLua.Core.Utf8 as Utf8

-- | Type definitions containing documented functions.
type DocumentedType e a = UDType e (DocumentedFunction e) a

-- | A userdata type, capturing the behavior of Lua objects that wrap
-- Haskell values. The type name must be unique; once the type has been
-- used to push or retrieve a value, the behavior can no longer be
-- modified through this type.
type DocumentedTypeWithList e a itemtype =
  UDTypeWithList e (DocumentedFunction e) a itemtype

-- | Defines a new type, defining the behavior of objects in Lua.
-- Note that the type name must be unique.
deftype :: LuaError e
        => Name                                 -- ^ type name
        -> [(Operation, DocumentedFunction e)]  -- ^ operations
        -> [Member e (DocumentedFunction e) a]  -- ^ methods
        -> DocumentedType e a
deftype = deftypeGeneric pushDocumentedFunction

-- | Defines a new type that could also be treated as a list; defines
-- the behavior of objects in Lua. Note that the type name must be
-- unique.
deftype' :: LuaError e
         => Name                                 -- ^ type name
         -> [(Operation, DocumentedFunction e)]  -- ^ operations
         -> [Member e (DocumentedFunction e) a]  -- ^ methods
         -> Maybe (ListSpec e a itemtype)  -- ^ list access
         -> DocumentedTypeWithList e a itemtype
deftype' = deftypeGeneric' pushDocumentedFunction

-- | Use a documented function as an object method.
method :: DocumentedFunction e
       -> Member e (DocumentedFunction e) a
method f = methodGeneric (functionName f) f

-- | Declares a new object operation from a documented function.
operation :: Operation             -- ^ the kind of operation
          -> DocumentedFunction e  -- ^ function used to perform the operation
          -> (Operation, DocumentedFunction e)
operation op f = (,) op $ setName (metamethodName op) f

-- | Defines a function parameter that takes the given type.
udparam :: LuaError e
        => DocumentedTypeWithList e a itemtype  -- ^ expected type
        -> Text            -- ^ parameter name
        -> Text            -- ^ parameter description
        -> Parameter e a
udparam ty = parameter (peekUD ty) (Utf8.toText . fromName $ udName ty)

-- | Defines a function result of the given type.
udresult :: LuaError e
         => DocumentedTypeWithList e a itemtype -- ^ result type
         -> Text           -- ^ result description
         -> FunctionResults e a
udresult ty = functionResult (pushUD ty) (Utf8.toText . fromName $ udName ty)
