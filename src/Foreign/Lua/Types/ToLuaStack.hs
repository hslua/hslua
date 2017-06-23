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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  ) where

import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Foreign.Lua.Functions
import Foreign.Ptr (FunPtr, Ptr)

-- | A value that can be pushed to the Lua stack.
class ToLuaStack a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua
  -- type.
  push :: a -> Lua ()

instance ToLuaStack LuaInteger where
  push = pushinteger

instance ToLuaStack LuaNumber where
  push = pushnumber

instance ToLuaStack Int where
  push = pushinteger . fromIntegral

instance ToLuaStack ByteString where
  push = pushstring

instance ToLuaStack Bool where
  push = pushboolean

instance ToLuaStack (FunPtr LuaCFunction) where
  push = pushcfunction

instance ToLuaStack (Ptr a) where
  push = pushlightuserdata

instance ToLuaStack () where
  push _ = pushnil

instance ToLuaStack a => ToLuaStack [a] where
  push xs = do
    let setField i x = push x *> rawseti (-2) i
    newtable
    zipWithM_ setField [1..] xs
