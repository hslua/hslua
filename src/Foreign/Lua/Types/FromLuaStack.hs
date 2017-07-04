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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
{-|
Module      : Foreign.Lua.Types.FromLuaStack
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module Foreign.Lua.Types.FromLuaStack
  ( FromLuaStack (..)
  , Result
  , peekEither
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.Monoid ((<>))
import Foreign.Lua.Types.Core
import Foreign.Lua.Functions
import Foreign.Ptr (FunPtr, Ptr)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Result returned when trying to get a value from the lua stack.
type Result a = Either String a

-- | A value that can be read from the Lua stack.
class FromLuaStack a where
  -- | Check if at index @n@ there is a convertible Lua value and if so return
  -- it wrapped in @Just@. Return @Nothing@ otherwise.
  peek :: StackIndex -> Lua a

instance FromLuaStack () where
  peek = typeChecked "nil" isnil (const $ return ())

instance FromLuaStack LuaInteger where
  peek = typeChecked "number" isnumber tointeger

instance FromLuaStack LuaNumber where
  peek = typeChecked "number" isnumber tonumber

instance FromLuaStack Int where
  peek = typeChecked "number" isnumber (fmap fromIntegral . tointeger)

instance FromLuaStack ByteString where
  peek = typeChecked "string" isstring tostring

instance FromLuaStack Bool where
  peek = typeChecked "boolean" isboolean toboolean

instance FromLuaStack (FunPtr LuaCFunction) where
  peek = typeChecked "C function" iscfunction tocfunction

instance FromLuaStack (Ptr a) where
  peek = typeChecked "user data" isuserdata touserdata

instance FromLuaStack LuaState where
  peek = typeChecked "LuaState (i.e., a thread)" isthread tothread

instance FromLuaStack T.Text where
  peek = fmap T.decodeUtf8 . peek

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} FromLuaStack [Char] where
#else
instance FromLuaStack String where
#endif
  peek = fmap T.unpack . peek

instance FromLuaStack a => FromLuaStack [a] where
  peek n = catchError (go . enumFromTo 1 =<< rawlen n) amendError
   where
    go [] = return []
    go (i : is) = do
      ret <- rawgeti n i *> peek (-1) <* pop 1
      (ret:) <$> go is
    amendError err = throwError ("Could not read list: " ++ err)

instance (Ord a, FromLuaStack a, FromLuaStack b) => FromLuaStack (Map a b) where
  peek idx = fromList <$> do
    pushnil
    remainingPairs
      where
        remainingPairs = do
          res <- nextPair (if idx < 0 then idx - 1 else idx)
          case res of
            Nothing -> return []
            Just a  -> (a:) <$> remainingPairs

-- | Get the next key-value pair from a table.
--
-- TODO: This function shows that there's a problem with the way we handle
-- errors.
nextPair :: (FromLuaStack a, FromLuaStack b)
         => StackIndex -> Lua (Maybe (a, b))
nextPair idx = do
  hasNext <- next idx
  if hasNext
    then do
      v <- peek (-1)
      k <- peek (-2)
      pop 1 -- removes the value, keeps the key
      return (Just (k, v))
    else return Nothing

-- | Use @test@ to check whether the value at stack index @n@ has the correct
-- type and use @peekfn@ to convert it to a haskell value if possible. A
-- successfully received value is wrapped using the @'Success'@ constructor,
-- while a type mismatch results in an @Error@ with the given error message.
typeChecked :: String
            -> (StackIndex -> Lua Bool)
            -> (StackIndex -> Lua a)
            -> StackIndex
            -> Lua a
typeChecked expectedType test peekfn n = do
  v <- test n
  if v
    then peekfn n
    else do
      actual <- ltype n >>= typename
      throwError $ "Expected a " <> expectedType <> " but got a " <> actual

peekEither :: FromLuaStack a => StackIndex -> Lua (Either String a)
peekEither idx = catchError (return <$> peek idx) (return . Left)
