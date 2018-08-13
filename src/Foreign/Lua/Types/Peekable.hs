{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.Types.Peekable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ScopedTypeVariables

Sending haskell objects to the lua stack.
-}
module Foreign.Lua.Types.Peekable
  ( Peekable (..)
  , safePeekKeyValuePairs
  , safePeekList
  , Result (..)
  , force
  , reportValueOnFailure
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.Set (Set)
import Data.Monoid ((<>))
import Foreign.Lua.Core as Lua
import Foreign.Ptr (Ptr)

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua.Utf8 as Utf8

-- | Result returned when trying to get a value from the lua stack.
data Result a
  = Success a
  | Error [ByteString]
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
force (Error ctx) = throwException (mconcat ctx)
{-# INLINABLE force #-}

-- | Use @test@ to check whether the value at stack index @n@ has the correct
-- type and use @peekfn@ to convert it to a haskell value if possible. A
-- successfully received value is wrapped using the @'Success'@ constructor,
-- while a type mismatch results in an @Error@ with the given error message.
typeChecked :: ByteString
            -> (StackIndex -> Lua Bool)
            -> (StackIndex -> Lua (Result a))
            -> StackIndex
            -> Lua (Result a)
typeChecked expectedType test peekfn idx = do
  v <- test idx
  if v then peekfn idx else mismatchError expectedType idx

-- | Report the expected and actual type of the value under the given index if
-- conversion failed.
reportValueOnFailure :: ByteString
                     -> (StackIndex -> Lua (Maybe a))
                     -> StackIndex -> Lua (Result a)
reportValueOnFailure expected peekMb idx = do
  res <- peekMb idx
  case res of
    (Just x) -> return (Success x)
    Nothing -> mismatchError expected idx

-- | Return a Result error containing a message about the assertion failure.
mismatchError :: ByteString -> StackIndex -> Lua (Result a)
mismatchError expected idx = do
  actualType <- ltype idx >>= typename
  actualValue <- tostring' idx <* pop 1
  let msg = "expected " <> expected
          <> ", got '" <> actualValue <> "' (" <> actualType <> ")"
  return (Error [msg])

-- | A value that can be read from the Lua stack.
class Peekable a where
  -- | Returns either the value at index @n@ of the Lua stack or a error
  -- message.
  safePeek :: StackIndex -> Lua (Result a)
  safePeek n = try (peek n) >>= \case
      Right x -> return $ Success x
      Left (Lua.Exception err) -> return $ Error [err]

  -- | Check if at index @n@ there is a convertible Lua value and if so return
  -- it.  Throws a @'Lua.Exception'@ otherwise.
  peek :: StackIndex -> Lua a
  peek n = safePeek n >>= force

instance Peekable () where
  safePeek = reportValueOnFailure "nil" $ \idx -> do
    isNil <- isnil idx
    return (if isNil then Just () else Nothing)

instance Peekable Lua.Integer where
  safePeek = reportValueOnFailure "integer" tointeger

instance Peekable Lua.Number where
  safePeek = reportValueOnFailure "number" tonumber

instance Peekable ByteString where
  safePeek = reportValueOnFailure "string" $ \idx -> do
    -- copy value, as tostring converts numbers to strings *in-place*.
    pushvalue idx
    tostring stackTop <* pop 1

instance Peekable Bool where
  safePeek = fmap return . toboolean

instance Peekable CFunction where
  safePeek = reportValueOnFailure "C function" tocfunction

instance Peekable (Ptr a) where
  safePeek = reportValueOnFailure "userdata" touserdata

instance Peekable Lua.State where
  safePeek = reportValueOnFailure "Lua state (i.e., a thread)" tothread

instance Peekable T.Text where
  safePeek = fmap (fmap Utf8.toText) . safePeek

instance Peekable BL.ByteString where
  safePeek = fmap (fmap BL.fromStrict) . safePeek

instance {-# OVERLAPS #-} Peekable [Char] where
  safePeek = fmap (fmap Utf8.toString) . safePeek

instance Peekable a => Peekable [a] where
  safePeek = safePeekList

instance (Ord a, Peekable a, Peekable b) => Peekable (Map a b) where
  safePeek idx = fmap fromList <$> safePeekKeyValuePairs idx

instance (Ord a, Peekable a) => Peekable (Set a) where
  safePeek idx = -- All keys with non-nil values are in the set
    fmap (Set.fromList . map fst . filter snd) <$> safePeekKeyValuePairs idx

-- | Read a table into a list
safePeekList :: Peekable a => StackIndex -> Lua (Result [a])
safePeekList = typeChecked "table" istable $ \idx -> do
  let elementsAt [] = return (Success [])
      elementsAt (i : is) = do
        ret <- rawgeti idx i *> safePeek (nthFromTop 1) <* pop 1
        case ret of
          Success x -> fmap (x:) <$> elementsAt is
          Error msgs -> return (Error msgs)
  listLength <- fromIntegral <$> rawlen idx
  inContext "Could not read list: " (elementsAt [1..listLength])

-- | Read a table into a list of pairs.
safePeekKeyValuePairs :: (Peekable a, Peekable b)
                      => StackIndex -> Lua (Result [(a, b)])
safePeekKeyValuePairs = typeChecked "table" istable $ \idx -> do
  let remainingPairs = do
        res <- nextPair (if idx < 0 then idx - 1 else idx)
        case res of
          Nothing -> return (Success [])
          Just (Success a)  -> fmap (a:) <$> remainingPairs
          Just (Error msgs) -> do
            pop 1  -- drop remaining key from stack
            return (Error msgs)
  inContext "Could not read key-value pairs: " $ do
    pushnil
    remainingPairs

-- | Get the next key-value pair from a table. Assumes the last key to be on the
-- top of the stack and the table at the given index @idx@.
nextPair :: (Peekable a, Peekable b)
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

inContext :: ByteString -> Lua (Result a) -> Lua (Result a)
inContext ctx op = do
  res <- op
  case res of
    Success _ -> return res
    Error msgs -> return $ Error (ctx : msgs)

--
-- Tuples
--

instance (Peekable a, Peekable b) => Peekable (a, b) where
  safePeek = typeChecked "table" istable $ \idx -> do
    a <- rawgeti idx 1 *> safePeek (-1) <* pop 1
    b <- rawgeti idx 2 *> safePeek (-1) <* pop 1
    return $ (,) <$> a <*> b

instance (Peekable a, Peekable b, Peekable c) =>
         Peekable (a, b, c)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    pop 4
    return $ (,,) <$> a <*> b <*> c

instance (Peekable a, Peekable b, Peekable c, Peekable d) =>
         Peekable (a, b, c, d)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    pop 5
    return $ (,,,) <$> a <*> b <*> c <*> d

instance (Peekable a, Peekable b, Peekable c,
          Peekable d, Peekable e) =>
         Peekable (a, b, c, d, e)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    pop 6
    return $ (,,,,) <$> a <*> b <*> c <*> d <*> e

instance (Peekable a, Peekable b, Peekable c,
          Peekable d, Peekable e, Peekable f) =>
         Peekable (a, b, c, d, e, f)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
    pushvalue idx
    a <- getTableIndex 1
    b <- getTableIndex 2
    c <- getTableIndex 3
    d <- getTableIndex 4
    e <- getTableIndex 5
    f <- getTableIndex 6
    pop 7
    return $ (,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f

instance (Peekable a, Peekable b, Peekable c, Peekable d,
          Peekable e, Peekable f, Peekable g) =>
         Peekable (a, b, c, d, e, f, g)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
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

instance (Peekable a, Peekable b, Peekable c, Peekable d,
          Peekable e, Peekable f, Peekable g, Peekable h) =>
         Peekable (a, b, c, d, e, f, g, h)
 where
  safePeek = typeChecked "table" istable $ \idx -> do
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
getTableIndex :: Peekable b => Lua.Integer -> Lua (Result b)
getTableIndex key = do
  let idx = nthFromTop (fromIntegral key)
  rawgeti idx key
  safePeek (nthFromTop 1)
