{-|
Module      : HsLua.Push
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which marshal and push Haskell values onto Lua's stack.
-}
module HsLua.Push
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
  -- * Collections
  , pushList
  , pushMap
  , pushSet
  ) where

import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Data.Map (Map, toList)
import Data.Set (Set)
import HsLua.Core as Lua
import Numeric (showGFloat)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified HsLua.Core.Utf8 as Utf8

-- | Function to push a value to Lua's stack.
type Pusher e a = a -> LuaE e ()

-- | Pushes a 'Bool' as a Lua boolean.
pushBool :: Pusher e Bool
pushBool = pushboolean

-- | Pushes a 'T.Text' value as an UTF-8 encoded string.
pushText :: Pusher e T.Text
pushText = pushstring . Utf8.fromText

-- | Pushes a 'ByteString' as a raw string.
pushByteString :: Pusher e ByteString
pushByteString = pushstring

-- | Pushes a lazy 'BL.ByteString' as a raw string.
pushLazyByteString :: Pusher e BL.ByteString
pushLazyByteString = pushstring . BL.toStrict

-- | Pushes a 'String' as an UTF-8 encoded Lua string.
pushString :: String -> LuaE e ()
pushString = pushstring . Utf8.fromString

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
pushKeyValuePairs pushKey pushValue m = do
  let addValue (k, v) = pushKey k *> pushValue v *> rawset (-3)
  newtable
  mapM_ addValue m

-- | Push list as numerically indexed table.
pushList :: LuaError e => Pusher e a -> [a] -> LuaE e ()
pushList push xs = do
  let setField i x = push x *> rawseti (-2) i
  newtable
  zipWithM_ setField [1..] xs

-- | Push 'Map' as default key-value Lua table.
pushMap :: LuaError e => Pusher e a -> Pusher e b -> Pusher e (Map a b)
pushMap pushKey pushValue m = pushKeyValuePairs pushKey pushValue $ toList m

-- | Push a 'Set' as idiomatic Lua set, i.e., as a table with the set
-- elements as keys and @true@ as values.
pushSet :: LuaError e => Pusher e a -> Pusher e (Set a)
pushSet pushElement set = do
  let addItem item = pushElement item *> pushboolean True *> rawset (-3)
  newtable
  mapM_ addItem set
