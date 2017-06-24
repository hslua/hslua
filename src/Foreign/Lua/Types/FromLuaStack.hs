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
  , Result (..)
  ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Foreign.Lua.Functions
import Foreign.Ptr (FunPtr, Ptr)

-- | Result returned when trying to get a value from the lua stack.
data Result a
  = Error String
  | Success a
  deriving (Eq, Ord, Show)

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Error err) = Error err

instance Applicative Result where
  pure  = Success
  (<*>) = ap

instance Monad Result where
  fail = Fail.fail
  return = pure

  Success x >>= k = k x
  Error err >>= _ = Error err

instance Fail.MonadFail Result where
  fail = Error

instance MonadPlus Result where
  mzero = empty
  mplus = (<|>)

instance Alternative Result where
  empty = fail "empty was called"
  a@(Success _) <|> _ = a
  _             <|> b = b


-- | A value that can be read from the Lua stack.
class FromLuaStack a where
  -- | Check if at index @n@ there is a convertible Lua value and if so return
  -- it wrapped in @Just@. Return @Nothing@ otherwise.
  peek :: StackIndex -> Lua (Result a)

instance FromLuaStack LuaInteger where
  peek = tryPeek "number" isnumber tointeger

instance FromLuaStack LuaNumber where
  peek = tryPeek "number" isnumber tonumber

instance FromLuaStack Int where
  peek = tryPeek "number" isnumber (fmap fromIntegral . tointeger)

instance FromLuaStack ByteString where
  peek = tryPeek "string" isstring tostring

instance FromLuaStack Bool where
  peek = tryPeek "boolean" isboolean toboolean

instance FromLuaStack (FunPtr LuaCFunction) where
  peek = tryPeek "C function" iscfunction tocfunction

instance FromLuaStack (Ptr a) where
  peek = tryPeek "user data" isuserdata touserdata

instance FromLuaStack LuaState where
  peek = tryPeek "LuaState (i.e., a thread)" isthread tothread

instance FromLuaStack a => FromLuaStack [a] where
  peek n = go . enumFromTo 1 =<< rawlen n
   where
    go [] = return $ Success []
    go (i : is) = do
      ret <- rawgeti n i *> peek (-1) <* pop 1
      case ret of
        Error err   -> return . Error $ "Could not read list: " <> err
        Success val -> fmap (val:) <$> go is

-- | Use @test@ to check whether the value at stack index @n@ has the correct
-- type and use @peekfn@ to convert it to a haskell value if possible. A
-- successfully received value is wrapped using the @'Success'@ constructor,
-- while a type mismatch results in an @Error@ with the given error message.
tryPeek :: String
        -> (StackIndex -> Lua Bool)
        -> (StackIndex -> Lua a)
        -> StackIndex
        -> Lua (Result a)
tryPeek expectedType test peekfn n = do
  v <- test n
  if v
    then Success <$> peekfn n
    else do
      actual <- ltype n >>= typename
      return . Error $ "Expected a " <> expectedType <> " but got a " <> actual
