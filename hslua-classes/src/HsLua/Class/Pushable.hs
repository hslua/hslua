{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Class.Pushable
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module HsLua.Class.Pushable
  ( Pushable (..)
  , pushList
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Set (Set)
import HsLua.Core as Lua
import HsLua.Marshalling hiding (pushList)
import Foreign.Ptr (Ptr)

import qualified Data.ByteString.Lazy as BL
import qualified HsLua.Marshalling as Push

-- | A value that can be pushed to the Lua stack.
class Pushable a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua
  -- type.
  push :: LuaError e => a -> LuaE e ()

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

instance Pushable Text where
  push = pushText

instance Pushable BL.ByteString where
  push = pushLazyByteString

instance Pushable Prelude.Integer where
  push = pushIntegral

instance Pushable Int where
  push = pushIntegral

instance Pushable Float where
  push = pushRealFloat

instance Pushable Double where
  push = pushRealFloat

instance {-# OVERLAPS #-} Pushable [Char] where
  push = pushString

instance Pushable a => Pushable [a] where
  push = Push.pushList push

instance (Pushable a, Pushable b) => Pushable (Map a b) where
  push = pushMap push push

instance Pushable a => Pushable (Set a) where
  push = pushSet push

-- | Push list as numerically indexed table.
pushList :: (LuaError e, Pushable a) => [a] -> LuaE e ()
pushList = Push.pushList push

--
-- Tuples
--
instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b) =>
  Pushable (a, b)
 where
  push (a, b) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b

instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c) =>
  Pushable (a, b, c)
 where
  push (a, b, c) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c

instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c, Pushable d) =>
  Pushable (a, b, c, d)
 where
  push (a, b, c, d) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d

instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c, Pushable d, Pushable e) =>
  Pushable (a, b, c, d, e)
 where
  push (a, b, c, d, e) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e

instance  {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c,
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

instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c, Pushable d,
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

instance {-# OVERLAPPABLE #-}
  (Pushable a, Pushable b, Pushable c, Pushable d,
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
addRawInt :: (LuaError e, Pushable a) => Lua.Integer -> a -> LuaE e ()
addRawInt idx val = do
  push val
  rawseti (-2) idx
