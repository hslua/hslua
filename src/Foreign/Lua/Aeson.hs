{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      :  Foreign.Lua.Aeson
Copyright   :  © 2017–2018 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Glue to hslua for aeson values.

This provides a @StackValue@ instance for aeson's @Value@ type. The following
conventions are used:

- @Null@ values are encoded as the special global @_NULL@. Using @Nil@ would
  cause problems with null-containing arrays.

- Objects are converted to tables in a straight-forward way.

- Arrays are converted to lua tables. Array-length is included as the value at
  index 0. This makes it possible to distinguish between empty arrays and empty
  objects.
-}
module Foreign.Lua.Aeson
  ( registerNull
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*>), (*>), (<*))
#endif
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Vector (Vector, fromList, toList)
import Foreign.Lua

import qualified Foreign.Lua as Lua
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector

-- Scientific
instance Pushable Scientific where
  push = pushnumber . toRealFloat

instance Peekable Scientific where
  peek n = fromFloatDigits <$> (peek n :: Lua Lua.Number)

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
    Aeson.Null -> getglobal "_NULL"

instance Peekable Aeson.Value where
  peek i = do
    ltype' <- ltype i
    case ltype' of
      TypeBoolean -> Aeson.Bool  <$> peek i
      TypeNumber -> Aeson.Number <$> peek i
      TypeString -> Aeson.String <$> peek i
      TypeTable -> do
        rawgeti i 0
        isInt <- isnumber (-1)
        pop 1
        if isInt
          then Aeson.Array <$> peek i
          else do
            rawlen' <- rawlen i
            if rawlen' > 0
              then Aeson.Array <$> peek i
              else do
                isNull <- isLuaNull i
                if isNull
                  then return Aeson.Null
                  else Aeson.Object <$> peek i
      TypeNil -> return Aeson.Null
      _    -> Lua.throwException ("Unexpected type: " ++ show ltype')

-- | Create a new lua state suitable for use with aeson values. This behaves
-- like @newstate@ in hslua, but initializes the @_NULL@ global. That variable
-- is used to encode null values.
registerNull :: Lua ()
registerNull = do
  createtable 0 0
  setglobal "_NULL"

-- | Check if the value under the given index is rawequal to @_NULL@.
isLuaNull :: StackIndex -> Lua Bool
isLuaNull i = do
  let i' = if i < 0 then i - 1 else i
  getglobal "_NULL"
  rawequal i' (-1) <* pop 1

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
    let xs = HashMap.toList hm
    let addValue (k, v) = push k *> push v *> rawset (-3)
    newtable
    mapM_ addValue xs
