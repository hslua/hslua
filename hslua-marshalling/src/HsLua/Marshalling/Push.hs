{-|
Module      : HsLua.Marshalling.Push
Copyright   : © 2020-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : Portable

Functions which marshal and push Haskell values onto Lua's stack.
-}
module HsLua.Marshalling.Push
  ( Pusher
  -- * Primitives
  , pushBool
  , pushIntegral
  , pushRealFloat
  -- * Strings
  , pushByteString
  , pushLazyByteString
  , pushString
  , pushText
  , pushName
  -- * Collections
  , pushList
  , pushNonEmpty
  , pushKeyValuePairs
  , pushMap
  , pushSet
  -- * Combinators
  , pushPair
  , pushTriple
  , pushAsTable
  ) where

import Control.Monad (forM_, zipWithM_)
import Data.ByteString (ByteString)
import Data.Map (Map, toList)
import Data.Set (Set)
import HsLua.Core as Lua
import Numeric (showGFloat)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NonEmpty
import qualified HsLua.Core.Utf8 as Utf8

-- | Function to push a value to Lua's stack.
type Pusher e a = a -> LuaE e ()

-- | Pushes a 'Bool' as a Lua boolean.
pushBool :: Pusher e Bool
pushBool = pushboolean

-- | Pushes a 'T.Text' value as a UTF-8 encoded string.
pushText :: Pusher e T.Text
pushText = pushstring . Utf8.fromText

-- | Pushes a 'ByteString' as a raw string.
pushByteString :: Pusher e ByteString
pushByteString = pushstring

-- | Pushes a lazy 'BL.ByteString' as a raw string.
pushLazyByteString :: Pusher e BL.ByteString
pushLazyByteString = pushstring . BL.toStrict

-- | Pushes a 'String' as a UTF-8 encoded Lua string.
pushString :: String -> LuaE e ()
pushString = pushstring . Utf8.fromString

-- | Pushes a 'Name' as a UTF-8 encoded Lua string.
pushName :: Name -> LuaE e ()
pushName (Name n) = pushByteString n

-- | Pushes an @Integer@ to the Lua stack. Values representable as Lua
-- integers are pushed as such; bigger integers are represented using
-- their string representation.
pushIntegral :: (Integral a, Show a) => a -> LuaE e ()
pushIntegral i =
  let maxInt = fromIntegral (maxBound :: Lua.Integer)
      minInt = fromIntegral (minBound :: Lua.Integer)
      i' = fromIntegral i :: Prelude.Integer
  in if i' >= minInt && i' <= maxInt
     then pushinteger $ fromIntegral i
     else pushString  $ show i

-- | Push a floating point number to the Lua stack. Uses a string
-- representation for all types which do not match the float properties
-- of the 'Lua.Number' type.
pushRealFloat :: RealFloat a => a -> LuaE e ()
pushRealFloat f =
  let
    number = 0 :: Lua.Number
    realFloatFitsInNumber = floatRadix number == floatRadix f
      && floatDigits number == floatDigits f
      && floatRange number == floatRange f
  in if realFloatFitsInNumber
     then pushnumber (realToFrac f :: Lua.Number)
     else pushString (showGFloat Nothing f "")

-- | Push list of pairs as default key-value Lua table.
pushKeyValuePairs :: LuaError e
                  => Pusher e a -> Pusher e b -> Pusher e [(a,b)]
pushKeyValuePairs pushKey pushValue m = checkstack 3 >>= \case
  False -> failLua "stack overflow while pushing key-value pairs"
  True  -> do
    let addValue (k, v) = pushKey k *> pushValue v *> rawset (-3)
    newtable
    mapM_ addValue m

-- | Push list as numerically indexed table.
pushList :: LuaError e => Pusher e a -> [a] -> LuaE e ()
pushList push xs = checkstack 2 >>= \case
  False -> failLua "stack overflow while pushing a list"
  True  -> do
    let setField i x = push x *> rawseti (-2) i
    newtable
    zipWithM_ setField [1..] xs

-- | Push non-empty list as numerically indexed table.
pushNonEmpty :: LuaError e => Pusher e a -> NonEmpty.NonEmpty a -> LuaE e ()
pushNonEmpty push = pushList push . NonEmpty.toList

-- | Push 'Map' as default key-value Lua table.
pushMap :: LuaError e => Pusher e a -> Pusher e b -> Pusher e (Map a b)
pushMap pushKey pushValue m = pushKeyValuePairs pushKey pushValue $ toList m

-- | Push a 'Set' as idiomatic Lua set, i.e., as a table with the set
-- elements as keys and @true@ as values.
pushSet :: LuaError e => Pusher e a -> Pusher e (Set a)
pushSet pushElement set = checkstack 3 >>= \case
  False -> failLua "stack overflow while pushing a set"
  True  -> do
    let addItem item = pushElement item *> pushboolean True *> rawset (-3)
    newtable
    mapM_ addItem set

--
-- Combinators
--
-- | Pushes an object as a table, defined by a list of
-- field-names/push-function pairs.
pushAsTable :: LuaError e
            => [(Name, a -> LuaE e ())]
            -> a -> LuaE e ()
pushAsTable props obj = do
  createtable 0 (length props)
  forM_ props $ \(name, pushValue) -> do
    pushName name
    pushValue obj
    rawset (nth 3)

-- | Pushes a pair of values as a two element list.
pushPair :: LuaError e
         => Pusher e a -> Pusher e b
         -> (a, b)
         -> LuaE e ()
pushPair pushA pushB (a,b) = do
  newtable
  pushA a
  rawseti (nth 2) 1
  pushB b
  rawseti (nth 2) 2

-- | Pushes a value triple as a three element list.
pushTriple :: LuaError e
           => Pusher e a -> Pusher e b -> Pusher e c
           -> (a, b, c)
           -> LuaE e ()
pushTriple pushA pushB pushC (a,b,c) = do
  newtable
  zipWithM_ (\p i -> p *> rawseti (nth 2) i)
            [pushA a, pushB b, pushC c]
            [1..]
