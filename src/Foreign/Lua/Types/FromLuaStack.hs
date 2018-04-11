{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017-2018 Albert Krewinkel

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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.Types.FromLuaStack
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module Foreign.Lua.Types.FromLuaStack
  ( FromLuaStack (..)
  , peekEither
  , pairsFromTable
  , toList
  , Result (..)
  , force
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.Set (Set)
import Data.Monoid ((<>))
import Foreign.Lua.Core
import Foreign.Ptr (Ptr)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

-- | Result returned when trying to get a value from the lua stack.
data Result a
  = Success a
  | Error [BS.ByteString]
  deriving (Eq, Functor, Show)

instance Applicative Result where
  pure = Success
  Success f <*> Success x = Success (f x)
  Success _ <*> Error msgs = Error msgs
  Error msgs <*> _ = Error msgs

instance Monad Result where
  return = pure
  Success x >>= f = f x
  Error err >>= _ = Error err

-- | Force evaulation of a result value, throwing an exception in case of an
-- error.
force :: Result a -> Lua a
force (Success x) = return x
force (Error ctx) = throwLuaError .
  mconcat $ map BS.unpack ctx

-- | Use @test@ to check whether the value at stack index @n@ has the correct
-- type and use @peekfn@ to convert it to a haskell value if possible. A
-- successfully received value is wrapped using the @'Success'@ constructor,
-- while a type mismatch results in an @Error@ with the given error message.
typeChecked :: String
            -> (StackIndex -> Lua Bool)
            -> (StackIndex -> Lua (Result a))
            -> StackIndex
            -> Lua (Result a)
typeChecked expectedType test peekfn n = do
  v <- test n
  if v
    then peekfn n
    else do
      actual <- ltype n >>= typename
      let msg = "Expected a " <> expectedType <> " but got a " <> actual
      return (Error [BS.pack msg])

typeChecked' :: String
             -> (StackIndex -> Lua Bool)
             -> (StackIndex -> Lua a)
             -> StackIndex -> Lua (Result a)
typeChecked' expectedType test peekfn =
  typeChecked expectedType test (fmap return . peekfn)

-- | A value that can be read from the Lua stack.
class FromLuaStack a where
  -- | Returns either the value at index @n@ of the Lua stack or a error
  -- message.
  safePeek :: StackIndex -> Lua (Result a)
  safePeek n = do
    res <- tryLua (peek n)
    case res of
      Right x -> return $ Success x
      Left (LuaException err) -> return $ Error [BS.pack err]

  -- | Check if at index @n@ there is a convertible Lua value and if so return
  -- it.  Throws a @'LuaException'@ otherwise.
  peek :: StackIndex -> Lua a
  peek n = safePeek n >>= force

-- | Try to convert the value at the given stack index to a haskell value.
-- Returns @Left@ with an error message on failure.
peekEither :: FromLuaStack a => StackIndex -> Lua (Either String a)
peekEither idx = safePeek idx >>= \case
  Success x -> return $ Right x
  Error msgs -> return . Left . mconcat $ map BS.unpack msgs

instance FromLuaStack () where
  safePeek = typeChecked' "nil" isnil (const $ return ())

instance FromLuaStack LuaInteger where
  safePeek = typeChecked' "number" isnumber tointeger

instance FromLuaStack LuaNumber where
  safePeek = typeChecked' "number" isnumber tonumber

instance FromLuaStack ByteString where
  safePeek = typeChecked' "string" isstring tostring

instance FromLuaStack Bool where
  safePeek = typeChecked' "boolean" isboolean toboolean

instance FromLuaStack CFunction where
  safePeek = typeChecked' "C function" iscfunction tocfunction

instance FromLuaStack (Ptr a) where
  safePeek = typeChecked' "user data" isuserdata touserdata

instance FromLuaStack LuaState where
  safePeek = typeChecked' "LuaState (i.e., a thread)" isthread tothread

instance FromLuaStack T.Text where
  safePeek = fmap (fmap T.decodeUtf8) . safePeek

instance FromLuaStack BL.ByteString where
  safePeek = fmap (fmap BL.fromStrict) . safePeek

instance {-# OVERLAPS #-} FromLuaStack [Char] where
  safePeek = fmap (fmap T.unpack) . safePeek

instance FromLuaStack a => FromLuaStack [a] where
  safePeek = typeChecked "table" istable toList

instance (Ord a, FromLuaStack a, FromLuaStack b) => FromLuaStack (Map a b) where
  safePeek idx = fmap fromList <$> pairsFromTable idx

instance (Ord a, FromLuaStack a) => FromLuaStack (Set a) where
  safePeek idx =
    fmap (Set.fromList . map fst . filter snd) <$> pairsFromTable idx

-- | Read a table into a list
toList :: FromLuaStack a => StackIndex -> Lua (Result [a])
toList n = inContext "Could not read list: " $
  go . enumFromTo 1 =<< rawlen n
 where
  go [] = return (Success [])
  go (i : is) = do
    ret <- rawgeti n i *> safePeek (nthFromTop 1) <* pop 1
    case ret of
      Success x -> fmap (x:) <$> go is
      Error msgs -> return (Error msgs)

-- | Read a table into a list of pairs.
pairsFromTable :: (FromLuaStack a, FromLuaStack b)
               => StackIndex -> Lua (Result [(a, b)])
pairsFromTable idx =
  inContext "Could not read key-value pairs: " $ do
    pushnil
    remainingPairs
 where
  remainingPairs = do
    res <- nextPair (if idx < 0 then idx - 1 else idx)
    case res of
      Nothing -> return (Success [])
      Just (Success a)  -> fmap (a:) <$> remainingPairs
      Just (Error msgs) -> do
        pop 1  -- drop remaining key from stack
        return (Error msgs)

-- | Get the next key-value pair from a table. Assumes the last key to be on the
-- top of the stack and the table at the given index @idx@.
nextPair :: (FromLuaStack a, FromLuaStack b)
         => StackIndex -> Lua (Maybe (Result (a, b)))
nextPair idx = do
  hasNext <- next idx
  if hasNext
    then do
      v <- safePeek (nthFromTop 1)
      k <- safePeek (nthFromTop 2)
      pop 1 -- removes the value, keeps the key
      return . Just $ (,) <$> k <*> v
    else return Nothing

inContext :: String -> Lua (Result a) -> Lua (Result a)
inContext ctx op = do
  res <- op
  case res of
    Success _ -> return res
    Error msgs -> return $ Error (BS.pack ctx : msgs)

--
-- Tuples
--

instance (FromLuaStack a, FromLuaStack b) => FromLuaStack (a, b) where
  safePeek idx = do
    a <- rawgeti idx 1 *> safePeek (-1) <* pop 1
    b <- rawgeti idx 2 *> safePeek (-1) <* pop 1
    return $ (,) <$> a <*> b

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c) =>
         FromLuaStack (a, b, c)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    pop 4
    return $ (,,) <$> a <*> b <*> c

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c, FromLuaStack d) =>
         FromLuaStack (a, b, c, d)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    pop 5
    return $ (,,,) <$> a <*> b <*> c <*> d

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c,
          FromLuaStack d, FromLuaStack e) =>
         FromLuaStack (a, b, c, d, e)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    pop 6
    return $ (,,,,) <$> a <*> b <*> c <*> d <*> e

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c,
          FromLuaStack d, FromLuaStack e, FromLuaStack f) =>
         FromLuaStack (a, b, c, d, e, f)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    f <- getTableIndex 6
    pop 7
    return $ (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c, FromLuaStack d,
          FromLuaStack e, FromLuaStack f, FromLuaStack g) =>
         FromLuaStack (a, b, c, d, e, f, g)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    f <- getTableIndex 6
    g <- getTableIndex 7
    pop 8
    return $ (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g

instance (FromLuaStack a, FromLuaStack b, FromLuaStack c, FromLuaStack d,
          FromLuaStack e, FromLuaStack f, FromLuaStack g, FromLuaStack h) =>
         FromLuaStack (a, b, c, d, e, f, g, h)
 where
  safePeek idx = do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    f <- getTableIndex 6
    g <- getTableIndex 7
    h <- getTableIndex 8
    pop 9
    return $ (,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h

-- | Helper function to get the nth table value
getTableIndex :: FromLuaStack b => Int -> Lua (Result b)
getTableIndex key = do
  let idx = nthFromTop (fromIntegral key)
  rawgeti idx key
  safePeek (nthFromTop 1)
