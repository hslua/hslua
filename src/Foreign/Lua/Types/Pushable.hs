{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.Types.Pushable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module Foreign.Lua.Types.Pushable
  ( Pushable (..)
  , pushList
  ) where

import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Data.Map (Map, toList)
import Data.Set (Set)
import Foreign.Lua.Core as Lua
import Foreign.Ptr (Ptr)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua.Utf8 as Utf8

-- | A value that can be pushed to the Lua stack.
class Pushable a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua
  -- type.
  push :: a -> Lua ()

instance Pushable () where
  push = const pushnil

instance Pushable Lua.Integer where
  push = pushinteger

instance Pushable Lua.Number where
  push = pushnumber

instance Pushable ByteString where
  push = pushstring

instance Pushable Bool where
  push = pushboolean

instance Pushable CFunction where
  push = pushcfunction

instance Pushable (Ptr a) where
  push = pushlightuserdata

instance Pushable T.Text where
  push = push . Utf8.fromText

instance Pushable BL.ByteString where
  push = push . BL.toStrict

instance Pushable Prelude.Integer where
  push = pushInteger

instance Pushable Int where
  push = pushInteger . fromIntegral

instance {-# OVERLAPS #-} Pushable [Char] where
  push = push . Utf8.fromString

instance Pushable a => Pushable [a] where
  push = pushList


-- | Push an @Int@ to the Lua stack. Numbers representable as Lua integers are
-- pushed as such; bigger integers are represented using their string
-- representation.
pushInteger :: Prelude.Integer -> Lua ()
pushInteger i =
  let maxInt = fromIntegral (maxBound :: Lua.Integer)
      minInt = fromIntegral (minBound :: Lua.Integer)
  in if i >= minInt && i <= maxInt
     then push (fromIntegral i :: Lua.Integer)
     else push (show i)

-- | Push list as numerically indexed table.
pushList :: Pushable a => [a] -> Lua ()
pushList xs = do
  let setField i x = push x *> rawseti (-2) i
  newtable
  zipWithM_ setField [1..] xs

instance (Pushable a, Pushable b) => Pushable (Map a b) where
  push m = do
    let addValue (k, v) = push k *> push v *> rawset (-3)
    newtable
    mapM_ addValue (toList m)

instance Pushable a => Pushable (Set a) where
  push set = do
    let addItem item = push item *> push True *> rawset (-3)
    newtable
    mapM_ addItem set

--
-- Tuples
--
instance (Pushable a, Pushable b) => Pushable (a, b) where
  push (a, b) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b

instance (Pushable a, Pushable b, Pushable c) =>
         Pushable (a, b, c)
 where
  push (a, b, c) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c

instance (Pushable a, Pushable b, Pushable c, Pushable d) =>
         Pushable (a, b, c, d)
 where
  push (a, b, c, d) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d

instance (Pushable a, Pushable b, Pushable c,
          Pushable d, Pushable e) =>
         Pushable (a, b, c, d, e)
 where
  push (a, b, c, d, e) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e

instance (Pushable a, Pushable b, Pushable c,
          Pushable d, Pushable e, Pushable f) =>
         Pushable (a, b, c, d, e, f)
 where
  push (a, b, c, d, e, f) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e
    addRawInt 6 f

instance (Pushable a, Pushable b, Pushable c, Pushable d,
          Pushable e, Pushable f, Pushable g) =>
         Pushable (a, b, c, d, e, f, g)
 where
  push (a, b, c, d, e, f, g) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e
    addRawInt 6 f
    addRawInt 7 g

instance (Pushable a, Pushable b, Pushable c, Pushable d,
          Pushable e, Pushable f, Pushable g, Pushable h) =>
         Pushable (a, b, c, d, e, f, g, h)
 where
  push (a, b, c, d, e, f, g, h) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e
    addRawInt 6 f
    addRawInt 7 g
    addRawInt 8 h

-- | Set numeric key/value in table at the top of the stack.
addRawInt :: Pushable a => Lua.Integer -> a -> Lua ()
addRawInt idx val = do
  push val
  rawseti (-2) idx
