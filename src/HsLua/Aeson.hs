{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
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

- Arrays are converted to lua tables. Array-length is included as the value at
  index 0. This makes it possible to distinguish between empty arrays and empty
  objects.

- JSON numbers are converted to Lua numbers (usually doubles), which can cause
  a loss of precision.
-}
module HsLua.Aeson
  ( pushNull
  ) where

import Control.Monad (when)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Vector (Vector, fromList, toList)
import Foreign.Lua as Lua

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector

-- Scientific
instance Pushable Scientific where
  push = pushnumber . toRealFloat

instance Peekable Scientific where
  peek = fmap (fromFloatDigits :: Lua.Number -> Scientific) . peek

-- Vector
instance (Pushable a) => Pushable (Vector a) where
  push = pushvector

instance (Peekable a) => Peekable (Vector a) where
  peek = tovector

-- HashMap
instance (Eq a, Hashable a, Pushable a, Pushable b)
      => Pushable (HashMap a b) where
  push = pushTextHashMap

instance (Eq a, Hashable a, Peekable a, Peekable b)
      => Peekable (HashMap a b) where
  peek = fmap HashMap.fromList . peekKeyValuePairs

-- | Hslua StackValue instance for the Aeson Value data type.
instance Pushable Aeson.Value where
  push = \case
    Aeson.Object o -> push o
    Aeson.Number n -> push n
    Aeson.String s -> push s
    Aeson.Array a -> push a
    Aeson.Bool b -> push b
    Aeson.Null -> pushNull

instance Peekable Aeson.Value where
  peek idx =
    ltype idx >>= \case
      TypeBoolean -> Aeson.Bool  <$> peek idx
      TypeNumber -> Aeson.Number <$> peek idx
      TypeString -> Aeson.String <$> peek idx
      TypeTable -> do
        rawgeti idx 0
        isInt <- isinteger stackTop
        pop 1
        if isInt
          then Aeson.Array <$> peek idx
          else do
            rawlen' <- rawlen idx
            if rawlen' > 0
              then Aeson.Array <$> peek idx
              else do
                isNull' <- isNull idx
                if isNull'
                  then return Aeson.Null
                  else Aeson.Object <$> peek idx
      TypeNil -> return Aeson.Null
      luaType -> Lua.throwException ("Unexpected type: " ++ show luaType)

-- | Registry key containing the representation for JSON null values.
nullRegistryField :: String
nullRegistryField = "HSLUA_AESON_NULL"

-- | Push the value which represents JSON null values to the stack (a specific
-- empty table by default). Internally, this uses the contents of the
-- @HSLUA_AESON_NULL@ registry field; modifying this field is possible, but it
-- must always be non-nil.
pushNull :: Lua ()
pushNull = do
  push nullRegistryField
  rawget registryindex
  uninitialized <- isnil stackTop
  when uninitialized $ do
    pop 1 -- remove nil
    newtable
    pushvalue stackTop
    setfield registryindex nullRegistryField

-- | Check if the value under the given index represents a @null@ value.
isNull :: StackIndex -> Lua Bool
isNull idx = do
  idx' <- absindex idx
  pushNull
  rawequal idx' stackTop <* pop 1

-- | Push a vector unto the stack.
pushvector :: Pushable a => Vector a -> Lua ()
pushvector v = do
  pushList . toList $ v
  push (fromIntegral (Vector.length v) :: Lua.Integer)
  rawseti (-2) 0

-- | Try reading the value under the given index as a vector.
tovector :: Peekable a => StackIndex -> Lua (Vector a)
tovector = fmap fromList . Lua.peekList

-- | Push a hashmap unto the stack.
pushTextHashMap :: (Pushable a, Pushable b) => HashMap a b -> Lua ()
pushTextHashMap hm = do
  let addValue (k, v) = push k *> push v *> rawset (-3)
  newtable
  mapM_ addValue (HashMap.toList hm)
