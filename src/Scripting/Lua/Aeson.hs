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
  ( module Scripting.Lua
  , newstate
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*>), (*>))
#endif
import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector, fromList, toList)
import Scripting.Lua (LuaState, StackValue)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector
import qualified Scripting.Lua as Lua

instance StackValue Scientific where
  push lua n = Lua.pushnumber lua (toRealFloat n)
  peek lua n = fmap fromFloatDigits <$>
               (Lua.peek lua n :: IO (Maybe Lua.LuaNumber))
  valuetype _ = Lua.TNUMBER

instance StackValue Text where
  push lua t = Lua.push lua (encodeUtf8 t)
  peek lua i = fmap decodeUtf8 <$> Lua.peek lua i
  valuetype _ = Lua.TSTRING

instance (StackValue a) => StackValue (Vector a) where
  push lua v = pushvector lua v
  peek lua i = tovector lua i
  valuetype _ = Lua.TTABLE

instance (Eq a, Hashable a, StackValue a, StackValue b)
      => StackValue (HashMap a b) where
  push lua h = pushTextHashMap lua h
  peek lua i = fmap HashMap.fromList <$> getPairs lua i
  valuetype _ = Lua.TTABLE

-- | Hslua StackValue instance for the Aeson Value data type.
instance StackValue Aeson.Value where
  push lua = \case
    Aeson.Object o -> Lua.push lua o
    Aeson.Number n -> Lua.push lua n
    Aeson.String s -> Lua.push lua s
    Aeson.Array a -> Lua.push lua a
    Aeson.Bool b -> Lua.push lua b
    Aeson.Null -> Lua.getglobal lua "_NULL"
  peek lua i = do
    ltype <- Lua.ltype lua i
    case ltype of
      Lua.TBOOLEAN -> fmap Aeson.Bool  <$> Lua.peek lua i
      Lua.TNUMBER -> fmap Aeson.Number <$> Lua.peek lua i
      Lua.TSTRING -> fmap Aeson.String <$> Lua.peek lua i
      Lua.TTABLE -> do
        Lua.rawgeti lua i 0
        len <- Lua.peek lua (-1)
        Lua.pop lua 1
        case (len :: Maybe Int) of
          Just _  -> fmap Aeson.Array <$> Lua.peek lua i
          Nothing -> do
            objlen <- Lua.objlen lua i
            if objlen > 0
              then fmap Aeson.Array <$> Lua.peek lua i
              else do
                isNull <- isLuaNull lua i
                if isNull
                  then return $ Just Aeson.Null
                  else fmap Aeson.Object <$> Lua.peek lua i
      Lua.TNIL -> return $ Just Aeson.Null
      _        -> error $ "Unexpected type: " ++ (show ltype)
  valuetype = \case
    Aeson.Object _ -> Lua.TTABLE
    Aeson.Number _ -> Lua.TNUMBER
    Aeson.String _ -> Lua.TSTRING
    Aeson.Array _ -> Lua.TTABLE
    Aeson.Bool _ -> Lua.TBOOLEAN
    Aeson.Null -> Lua.TTABLE

-- | Create a new lua state suitable for use with aeson values. This behaves
-- like @newstate@ in hslua, but initializes the @_NULL@ global. That variable
-- is used to encode null values.
newstate :: IO LuaState
newstate = do
  lua <- Lua.newstate
  Lua.createtable lua 0 0
  Lua.setglobal lua "_NULL"
  return lua

-- | Check if the value under the given index is lua-equal to @_NULL@.
isLuaNull :: LuaState -> Int -> IO Bool
isLuaNull lua i = do
  let i' = if i < 0 then i - 1 else i
  Lua.getglobal lua "_NULL"
  res <- Lua.equal lua i' (-1)
  Lua.pop lua 1
  return res

-- | Push a vector unto the stack.
pushvector :: StackValue a => LuaState -> Vector a -> IO ()
pushvector lua v = do
  Lua.pushlist lua . toList $ v
  Lua.push lua (Vector.length v)
  Lua.rawseti lua (-2) 0

-- | Try reading the value under the given index as a vector.
tovector :: StackValue a => LuaState -> Int -> IO (Maybe (Vector a))
tovector = fmap (fmap (fmap fromList)) . Lua.tolist

-- | Try reading the value under the given index as a list of key-value pairs.
getPairs :: (StackValue a, StackValue b)
         => LuaState -> Int -> IO (Maybe [(a, b)])
getPairs lua t = do
  Lua.pushnil lua
  pairs <- sequence <$> remainingPairs
  return pairs
 where
  t' = if t < 0 then t - 1 else t
  remainingPairs = do
    res <- nextPair
    case res of
      Nothing -> return []
      Just a  -> (a:) <$> remainingPairs
  nextPair = do
    hasNext <- Lua.next lua t'
    if hasNext
      then do
        val <- Lua.peek lua (-1)
        key <- Lua.peek lua (-2)
        Lua.pop lua 1 -- removes the value, keeps the key
        return $ Just <$> ((,) <$> key <*> val)
      else do
        return Nothing

-- | Push a hashmap unto the stack.
pushTextHashMap :: (StackValue a, StackValue b) => LuaState -> HashMap a b -> IO ()
pushTextHashMap lua hm = do
    let xs = HashMap.toList hm
    Lua.createtable lua (length xs + 1) 0
    let addValue (k, v) = Lua.push lua k *> Lua.push lua v *>
                          Lua.rawset lua (-3)
    mapM_ addValue xs
