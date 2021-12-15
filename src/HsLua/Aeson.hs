{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      :  HsLua.Aeson
Copyright   :  © 2017–2021 Albert Krewinkel
License     :  MIT
Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>

Glue to HsLua for aeson values.

This provides a @StackValue@ instance for aeson's @Value@ type. The following
conventions are used:

- @Null@ values are encoded as a special value (stored in the registry field
  @HSLUA_AESON_NULL@). Using @nil@ would cause problems with null-containing
  arrays.

- Objects are converted to tables in a straight-forward way.

- Arrays are converted to Lua tables. Array-length is included as the value at
  index 0. This makes it possible to distinguish between empty arrays and empty
  objects.

- JSON numbers are converted to Lua numbers (usually doubles), which can cause
  a loss of precision.
-}
module HsLua.Aeson
  ( peekValue
  , pushValue
  , peekVector
  , pushVector
  , pushNull
  , peekScientific
  , pushScientific
  , peekTextMap
  , pushTextMap
  ) where

import Control.Monad ((<$!>), when)
import Data.HashMap.Lazy (HashMap)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Vector (Vector, fromList, toList)
import HsLua as Lua

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector

-- Scientific
pushScientific :: LuaError e => Pusher e Scientific
pushScientific = pushRealFloat @Double . toRealFloat

peekScientific :: LuaError e => Peeker e Scientific
peekScientific =
  fmap fromFloatDigits . peekRealFloat @Double

peekTextMap :: LuaError e => Peeker e (HashMap Text Aeson.Value)
peekTextMap = fmap HashMap.fromList . peekKeyValuePairs peekText peekValue

-- | Hslua StackValue instance for the Aeson Value data type.
pushValue :: LuaError e => Pusher e Aeson.Value
pushValue = \case
  Aeson.Object o -> pushTextMap o
  Aeson.Number n -> pushScientific n
  Aeson.String s -> pushText s
  Aeson.Array a -> pushVector a
  Aeson.Bool b -> pushBool b
  Aeson.Null -> pushNull

peekValue :: LuaError e => Peeker e Aeson.Value
peekValue idx = liftLua (ltype idx) >>= \case
  TypeBoolean -> Aeson.Bool  <$!> peekBool idx
  TypeNumber -> Aeson.Number <$!> peekScientific idx
  TypeString -> Aeson.String <$!> peekText idx
  TypeTable -> do
    isInt <- liftLua $ rawgeti idx 0 *> isinteger top <* pop 1
    if isInt
      then Aeson.Array <$!> peekVector idx
      else do
        rawlen' <- liftLua $ rawlen idx
        if rawlen' > 0
          then Aeson.Array <$!> peekVector idx
          else do
            isNull' <- liftLua $ isNull idx
            if isNull'
              then return Aeson.Null
              else Aeson.Object <$!> peekTextMap idx
  TypeNil -> return Aeson.Null
  luaType -> fail ("Unexpected type: " ++ show luaType)

-- | Registry key containing the representation for JSON null values.
nullRegistryField :: Name
nullRegistryField = "HSLUA_AESON_NULL"

-- | Push the value which represents JSON null values to the stack (a specific
-- empty table by default). Internally, this uses the contents of the
-- @HSLUA_AESON_NULL@ registry field; modifying this field is possible, but it
-- must always be non-nil.
pushNull :: LuaError e => LuaE e ()
pushNull = do
  pushName nullRegistryField
  rawget registryindex
  uninitialized <- isnil top
  when uninitialized $ do
    pop 1 -- remove nil
    newtable
    pushvalue top
    setfield registryindex nullRegistryField

-- | Check if the value under the given index represents a @null@ value.
isNull :: LuaError e => StackIndex -> LuaE e Bool
isNull idx = do
  idx' <- absindex idx
  pushNull
  rawequal idx' top <* pop 1

-- | Push a vector onto the stack.
pushVector :: LuaError e => Pusher e (Vector Aeson.Value)
pushVector v = do
  pushList pushValue $ toList v
  pushIntegral (Vector.length v)
  rawseti (nth 2) 0

-- | Try reading the value under the given index as a vector.
peekVector :: LuaError e => Peeker e (Vector Aeson.Value)
peekVector = fmap fromList . Lua.peekList peekValue

-- | Push a hashmap onto the stack.
pushTextMap :: LuaError e => Pusher e (HashMap Text Aeson.Value)
pushTextMap = pushKeyValuePairs pushText pushValue . HashMap.toList
