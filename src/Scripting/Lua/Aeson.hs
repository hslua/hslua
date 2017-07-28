{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      :  Scripting.Lua.Aeson
Copyright   :  © 2017 Albert Krewinkel
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
module Scripting.Lua.Aeson
  ( registerNull
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*>), (*>))
#endif
import Control.Monad (zipWithM_)
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Vector (Vector, fromList, toList)
import Foreign.Lua hiding (newstate)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector
import qualified Foreign.Lua as Lua

-- Scientific
instance ToLuaStack Scientific where
  push n = pushnumber (toRealFloat n)

instance FromLuaStack Scientific where
  peek n = fromFloatDigits <$> (peek n :: Lua LuaNumber)

-- Vector
instance (ToLuaStack a) => ToLuaStack (Vector a) where
  push v = pushvector v

instance (FromLuaStack a) => FromLuaStack (Vector a) where
  peek i = tovector i

-- HashMap
instance (Eq a, Hashable a, ToLuaStack a, ToLuaStack b)
      => ToLuaStack (HashMap a b) where
  push h = pushTextHashMap h

instance (Eq a, Hashable a, FromLuaStack a, FromLuaStack b)
      => FromLuaStack (HashMap a b) where
  peek i = HashMap.fromList <$> pairsFromTable i

-- | Hslua StackValue instance for the Aeson Value data type.
instance ToLuaStack Aeson.Value where
  push = \case
    Aeson.Object o -> push o
    Aeson.Number n -> push n
    Aeson.String s -> push s
    Aeson.Array a -> push a
    Aeson.Bool b -> push b
    Aeson.Null -> getglobal "_NULL"

instance FromLuaStack Aeson.Value where
  peek i = do
    ltype' <- ltype i
    case ltype' of
      TBOOLEAN -> Aeson.Bool  <$> peek i
      TNUMBER -> Aeson.Number <$> peek i
      TSTRING -> Aeson.String <$> peek i
      TTABLE -> do
        rawgeti i 0
        isInt <- isnumber (-1)
        pop 1
        if isInt
          then Aeson.Array <$> peek i
          else do
            objlen <- rawlen i
            if objlen > 0
              then Aeson.Array <$> peek i
              else do
                isNull <- isLuaNull i
                if isNull
                  then return Aeson.Null
                  else Aeson.Object <$> peek i
      TNIL -> return Aeson.Null
      _    -> error $ "Unexpected type: " ++ (show ltype')

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
pushvector :: ToLuaStack a => Vector a -> Lua ()
pushvector v = do
  pushList . toList $ v
  push (Vector.length v)
  rawseti (-2) 0

-- | Try reading the value under the given index as a vector.
tovector :: FromLuaStack a => StackIndex -> Lua (Vector a)
tovector = fmap fromList . peekList

-- | Push a hashmap unto the stack.
pushTextHashMap :: (ToLuaStack a, ToLuaStack b) => HashMap a b -> Lua ()
pushTextHashMap hm = do
    let xs = HashMap.toList hm
    let addValue (k, v) = push k *> push v *> rawset (-3)
    newtable
    mapM_ addValue xs

pushList :: ToLuaStack a => [a] -> Lua ()
pushList xs = do
  let setField i x = push x *> rawseti (-2) i
  newtable
  zipWithM_ setField [1..] xs

-- | Read a table into a list
peekList :: FromLuaStack a => StackIndex -> Lua [a]
peekList n = (go . enumFromTo 1 =<< rawlen n) `catchLuaError` amendError
 where
  go [] = return []
  go (i : is) = do
    ret <- rawgeti n i *> peek (-1) <* pop 1
    (ret:) <$> go is
  amendError err = throwLuaError ("Could not read list: " ++ show err)
