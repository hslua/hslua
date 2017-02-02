{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-}
module Scripting.Lua.Aeson
  ( module Scripting.Lua
  ) where

import Data.HashMap.Lazy (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector, fromList, toList)
import Scripting.Lua (LuaState, StackValue)

import qualified Data.HashMap.Lazy as HashMap
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

pushvector :: StackValue a => LuaState -> Vector a -> IO ()
pushvector lua = Lua.pushlist lua . toList

tovector :: StackValue a => LuaState -> Int -> IO (Maybe (Vector a))
tovector = fmap (fmap (fmap fromList)) . Lua.tolist

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

pushTextHashMap :: (StackValue a, StackValue b) => LuaState -> HashMap a b -> IO ()
pushTextHashMap lua hm = do
    let xs = HashMap.toList hm
    Lua.createtable lua (length xs + 1) 0
    let addValue (k, v) = Lua.push lua k >> Lua.push lua v >>
                          Lua.rawset lua (-3)
    mapM_ addValue xs
