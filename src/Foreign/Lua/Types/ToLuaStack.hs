{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
{-|
Module      : Foreign.Lua.Types.ToLuaStack
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module Foreign.Lua.Types.ToLuaStack
  ( ToLuaStack (..)
  , pushList
  ) where

import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Data.Map (Map, toList)
import Data.Set (Set)
import Foreign.Lua.Api
import Foreign.Lua.Types.Lua
import Foreign.Ptr (Ptr)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

-- | A value that can be pushed to the Lua stack.
class ToLuaStack a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua
  -- type.
  push :: a -> Lua ()

instance ToLuaStack () where
  push = const pushnil

instance ToLuaStack LuaInteger where
  push = pushinteger

instance ToLuaStack LuaNumber where
  push = pushnumber

instance ToLuaStack ByteString where
  push = pushstring

instance ToLuaStack Bool where
  push = pushboolean

instance ToLuaStack CFunction where
  push = pushcfunction

instance ToLuaStack (Ptr a) where
  push = pushlightuserdata

instance ToLuaStack T.Text where
  push = push . T.encodeUtf8

instance ToLuaStack BL.ByteString where
  push = push . BL.toStrict

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} ToLuaStack [Char] where
#else
instance ToLuaStack [Char] where
#endif
  push = push . T.pack

instance ToLuaStack a => ToLuaStack [a] where
  push = pushList

-- | Push list as numerically indexed table.
pushList :: ToLuaStack a => [a] -> Lua ()
pushList xs = do
  let setField i x = push x *> rawseti (-2) i
  newtable
  zipWithM_ setField [1..] xs

instance (ToLuaStack a, ToLuaStack b) => ToLuaStack (Map a b) where
  push m = do
    let addValue (k, v) = push k *> push v *> rawset (-3)
    newtable
    mapM_ addValue (toList m)

instance ToLuaStack a => ToLuaStack (Set a) where
  push set = do
    let addItem item = push item *> pushboolean True *> rawset (-3)
    newtable
    mapM_ addItem (Set.toList set)

--
-- Tuples
--
instance (ToLuaStack a, ToLuaStack b) => ToLuaStack (a, b) where
  push (a, b) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c) =>
         ToLuaStack (a, b, c)
 where
  push (a, b, c) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c, ToLuaStack d) =>
         ToLuaStack (a, b, c, d)
 where
  push (a, b, c, d) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c,
          ToLuaStack d, ToLuaStack e) =>
         ToLuaStack (a, b, c, d, e)
 where
  push (a, b, c, d, e) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c,
          ToLuaStack d, ToLuaStack e, ToLuaStack f) =>
         ToLuaStack (a, b, c, d, e, f)
 where
  push (a, b, c, d, e, f) = do
    newtable
    addRawInt 1 a
    addRawInt 2 b
    addRawInt 3 c
    addRawInt 4 d
    addRawInt 5 e
    addRawInt 6 f

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c, ToLuaStack d,
          ToLuaStack e, ToLuaStack f, ToLuaStack g) =>
         ToLuaStack (a, b, c, d, e, f, g)
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

instance (ToLuaStack a, ToLuaStack b, ToLuaStack c, ToLuaStack d,
          ToLuaStack e, ToLuaStack f, ToLuaStack g, ToLuaStack h) =>
         ToLuaStack (a, b, c, d, e, f, g, h)
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
addRawInt :: ToLuaStack a => Int -> a -> Lua ()
addRawInt idx val = do
  push val
  rawseti (-2) idx
