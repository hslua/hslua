{-|
Module      : Foreign.Lua.Push
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which marshal and push Haskell values onto Lua's stack.
-}
module Foreign.Lua.Push
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
import Foreign.Lua.Core as Lua

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua.Utf8 as Utf8

type Pusher a = a -> Lua ()

-- | Pushes a 'Bool' as a Lua boolean.
pushBool :: Pusher Bool
pushBool = pushboolean

-- | Pushes a 'T.Text' value as an UTF-8 encoded string.
pushText :: Pusher T.Text
pushText = pushstring . Utf8.fromText

-- | Pushes a 'ByteString' as a raw string.
pushByteString :: Pusher ByteString
pushByteString = pushstring

-- | Pushes a lazy 'BL.ByteString' as a raw string.
pushLazyByteString :: Pusher BL.ByteString
pushLazyByteString = pushstring . BL.toStrict

-- | Pushes a 'String' as an UTF-8 encoded Lua string.
pushString :: String -> Lua ()
pushString = pushstring . Utf8.fromString

-- | Pushes an @Integer@ to the Lua stack. Values representable as Lua
-- integers are pushed as such; bigger integers are represented using
-- their string representation.
pushIntegral :: (Integral a, Show a) => a -> Lua ()
pushIntegral i =
  let maxInt = fromIntegral (maxBound :: Lua.Integer)
      minInt = fromIntegral (minBound :: Lua.Integer)
  in if i >= minInt && i <= maxInt
     then pushinteger $ fromIntegral i
     else pushString  $ show i

-- | Push a floating point number to the Lua stack.
pushRealFloat :: (RealFloat a, Show a) => a -> Lua ()
pushRealFloat f =
  let
    number = 0 :: Lua.Number
    doubleFitsInNumber = floatRadix number == floatRadix f
      && floatDigits number == floatDigits f
      && floatRange number == floatRange f
  in if doubleFitsInNumber
     then pushnumber (realToFrac f :: Lua.Number)
     else pushString (show f)

-- | Push list as numerically indexed table.
pushList :: Pusher a -> [a] -> Lua ()
pushList push xs = do
  let setField i x = push x *> rawseti (-2) i
  newtable
  zipWithM_ setField [1..] xs

-- | Push 'Map' as default key-value Lua table.
pushMap :: Pusher a -> Pusher b -> Pusher (Map a b)
pushMap pushKey pushValue m = do
  let addValue (k, v) = pushKey k *> pushValue v *> rawset (-3)
  newtable
  mapM_ addValue (toList m)

-- | Push a 'Set' as idiomatic Lua set, i.e., as a table with the set
-- elements as keys and @true@ as values.
pushSet :: Pusher a -> Pusher (Set a)
pushSet pushElement set = do
  let addItem item = pushElement item *> pushboolean True *> rawset (-3)
  newtable
  mapM_ addItem set
