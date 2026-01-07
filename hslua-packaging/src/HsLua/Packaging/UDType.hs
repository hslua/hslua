{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.UDType
Copyright   : © 2020-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

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
  , property'
  , possibleProperty
  , possibleProperty'
  , readonly
  , readonly'
  , alias
  , operation
  , peekUD
  , pushUD
  , initType  -- Reexported from ObjectOrientation
  , udparam
  , udresult
  , udTypeSpec
    -- * Helper types for building
  , Member
  , Operation (..)
  , Property
  , Possible (..)
  ) where

import Data.Map (Map)
import Data.Text (Text)
import HsLua.Core
import HsLua.Marshalling
import HsLua.ObjectOrientation
import HsLua.ObjectOrientation.Operation (metamethodName)
import HsLua.Packaging.Function
import HsLua.Packaging.Types (setName)
import HsLua.Typing (pushTypeSpec)
import qualified Data.Map as Map

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
deftype name ops methods = addDocHooks $
  deftypeGeneric' pushDocumentedFunction name ops methods emptyHooks

-- | Defines a new type that could also be treated as a list; defines
-- the behavior of objects in Lua. Note that the type name must be
-- unique.
deftype' :: LuaError e
         => Name                                 -- ^ type name
         -> [(Operation, DocumentedFunction e)]  -- ^ operations
         -> [Member e (DocumentedFunction e) a]  -- ^ methods
         -> Maybe (ListSpec e a itemtype)  -- ^ list access
         -> DocumentedTypeWithList e a itemtype
deftype' name ops methods mlistSpec = addDocHooks .
  deftypeGeneric' pushDocumentedFunction name ops methods $
    maybe emptyHooks listExtension mlistSpec

-- | Use a documented function as an object method.
method :: DocumentedFunction e
       -> Member e (DocumentedFunction e) a
method f = methodGeneric (functionName f) f

-- | Declares a new object operation from a documented function.
operation :: Operation             -- ^ the kind of operation
          -> DocumentedFunction e  -- ^ function used to perform the operation
          -> (Operation, DocumentedFunction e)
operation op f = (,) op $ f `setName` metamethodName op

-- | Defines a function parameter that takes the given type.
udparam :: LuaError e
        => DocumentedTypeWithList e a itemtype  -- ^ expected type
        -> Text            -- ^ parameter name
        -> Text            -- ^ parameter description
        -> Parameter e a
udparam ty = parameter (peekUDGeneric ty) (udTypeSpec ty)

-- | Defines a function result of the given type.
udresult :: LuaError e
         => DocumentedTypeWithList e a itemtype -- ^ result type
         -> Text           -- ^ result description
         -> FunctionResults e a
udresult ty = functionResult (pushUD ty) (udTypeSpec ty)

-- | Pushes a userdata value of the given type.
pushUD :: LuaError e => DocumentedTypeWithList e a itemtype -> a -> LuaE e ()
pushUD = pushUDGeneric

-- | Retrieves a userdata value of the given type.
peekUD :: LuaError e => DocumentedTypeWithList e a itemtype -> Peeker e a
peekUD = peekUDGeneric

-- | Adds a hook to the type that pushes the documentation.
addDocHooks
  :: LuaError e
  => UDTypeGeneric e (DocumentedFunction e) a
  -> UDTypeGeneric e (DocumentedFunction e) a
addDocHooks ty =
  let hooks = udHooks ty
  in ty
     { udHooks = hooks
       { hookMetatableSetup = do
           hookMetatableSetup hooks
           pushUDTypeDocs ty
       }
     }

-- | Pushes a documentation table for the given UD type.
pushUDTypeDocs :: LuaError e
               => DocumentedTypeWithList e a itemtype
               -> LuaE e ()
pushUDTypeDocs ty = do
  -- metadata table is at the top of the stack
  pushName "docs"
  pushAsTable
    [ ("name", pushName . udName)
    , ("properties", pushPropertyDocs . udProperties)
    ] ty
  rawset (nth 3)

pushPropertyDocs :: LuaError e
                 => Map Name (Property e a)
                 -> LuaE e ()
pushPropertyDocs = pushKeyValuePairs pushName pushPropDocs . Map.toList
  where
    pushPropDocs = pushAsTable
      [ ("description", pushText . propertyDescription)
      , ("type", pushTypeSpec . propertyType)
      ]
