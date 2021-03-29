{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.Class.Peekable
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Sending haskell objects to the lua stack.
-}
module HsLua.Class.Peekable
  ( Peekable (..)
  , PeekError (..)
  , peekKeyValuePairs
  , peekList
  , reportValueOnFailure
  , inContext
  ) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import Data.Set (Set)
import HsLua.Core as Lua
import Foreign.Ptr (Ptr)

import qualified Control.Monad.Catch as Catch
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified HsLua.Marshalling as Peek

-- | Use @test@ to check whether the value at stack index @n@ has the
-- correct type and use @peekfn@ to convert it to a haskell value if
-- possible. Throws and exception if the test failes with the expected
-- type name as part of the message.
typeChecked :: forall e a. LuaError e
            => ByteString                  -- ^ expected type
            -> (StackIndex -> LuaE e Bool) -- ^ pre-condition Checker
            -> (StackIndex -> LuaE e a)    -- ^ retrieval function
            -> StackIndex -> LuaE e a
typeChecked expectedType test peekfn idx = do
  v <- test idx
  if v
    then peekfn idx
    else throwTypeMismatchError expectedType idx

-- | Report the expected and actual type of the value under the given
-- index if conversion failed.
reportValueOnFailure :: forall e a. PeekError e
                     => ByteString
                     -> (StackIndex -> LuaE e (Maybe a))
                     -> StackIndex -> LuaE e a
reportValueOnFailure expected peekMb idx = do
  res <- peekMb idx
  case res of
    (Just x) -> return x
    Nothing  -> throwTypeMismatchError expected idx

-- | A value that can be read from the Lua stack.
class Peekable a where
  -- | Check if at index @n@ there is a convertible Lua value and if so return
  -- it.  Throws a @'Lua.Exception'@ otherwise.
  peek :: PeekError e => StackIndex -> LuaE e a

instance Peekable () where
  peek = reportValueOnFailure "nil" $ \idx -> do
    isNil <- isnil idx
    return (if isNil then Just () else Nothing)

instance Peekable Lua.Integer where
  peek = reportValueOnFailure "integer" tointeger

instance Peekable Lua.Number where
  peek = reportValueOnFailure "number" tonumber

instance Peekable ByteString where
  peek = Peek.peekByteString >=> Peek.force

instance Peekable Bool where
  peek = toboolean

instance Peekable CFunction where
  peek = reportValueOnFailure "C function" tocfunction

instance Peekable (Ptr a) where
  peek = reportValueOnFailure "userdata" touserdata

instance Peekable Lua.State where
  peek = reportValueOnFailure "Lua state (i.e., a thread)" tothread

instance Peekable T.Text where
  peek = Peek.peekText >=> Peek.force

instance Peekable BL.ByteString where
  peek = Peek.peekLazyByteString >=> Peek.force

instance Peekable Prelude.Integer where
  peek = Peek.peekIntegral >=> Peek.force

instance Peekable Int where
  peek = Peek.peekIntegral >=> Peek.force

instance Peekable Float where
  peek = Peek.peekRealFloat >=> Peek.force

instance Peekable Double where
  peek = Peek.peekRealFloat >=> Peek.force

instance {-# OVERLAPS #-} Peekable [Char] where
  peek = Peek.peekString >=> Peek.force

instance Peekable a => Peekable [a] where
  peek = peekList

instance (Ord a, Peekable a, Peekable b) => Peekable (Map a b) where
  peek = fmap fromList . peekKeyValuePairs

instance (Ord a, Peekable a) => Peekable (Set a) where
  peek = -- All keys with non-nil values are in the set
    fmap (Set.fromList . map fst . filter snd) . peekKeyValuePairs

-- | Read a table into a list
peekList :: (PeekError e, Peekable a) => StackIndex -> LuaE e [a]
peekList = typeChecked "table" istable $ \idx -> do
  let elementsAt [] = return []
      elementsAt (i : is) = do
        x <- (rawgeti idx i *> peek top) `Catch.finally` pop 1
        (x:) <$> elementsAt is
  listLength <- fromIntegral <$> rawlen idx
  inContext "Could not read list:" (elementsAt [1..listLength])

-- | Read a table into a list of pairs.
peekKeyValuePairs :: (Peekable a, Peekable b, PeekError e)
                  => StackIndex -> LuaE e [(a, b)]
peekKeyValuePairs = typeChecked "table" istable $ \idx -> do
  let remainingPairs = do
        res <- nextPair (if idx < 0 then idx - 1 else idx)
        case res of
          Nothing -> [] <$ return ()
          Just a  -> (a:) <$> remainingPairs
  pushnil
  remainingPairs
    -- ensure the remaining key is removed from the stack on exception
    `Catch.onException` pop 1

-- | Get the next key-value pair from a table. Assumes the last key to be on the
-- top of the stack and the table at the given index @idx@.
nextPair :: (PeekError e, Peekable a, Peekable b)
         => StackIndex -> LuaE e (Maybe (a, b))
nextPair idx = do
  hasNext <- next idx
  if hasNext
    then let pair = (,) <$> inContext "Could not read key of key-value pair:"
                                      (peek (nth 2))
                        <*> inContext "Could not read value of key-value pair:"
                                      (peek (nth 1))
         in Just <$> pair `Catch.finally` pop 1
            -- removes the value, keeps the key
    else return Nothing

-- | Specify a name for the context in which a computation is run. The
-- name is added to the error message in case of an exception.
inContext :: forall e a. PeekError e
          => String -> LuaE e a -> LuaE e a
inContext ctx op = try op >>= \case
  Right x  -> return x
  Left (err :: e) -> Catch.throwM $
    luaException @e (ctx ++ "\n\t" ++ messageFromException err)

-- | Exceptions that are to be used with 'peek' and similar functions
-- must be instances of this class. It ensures that error can be amended
-- with the context in which they happened.
class LuaError e => PeekError e where
  messageFromException :: e -> String

instance PeekError Lua.Exception where
  messageFromException = Lua.exceptionMessage

--
-- Tuples
--

instance (Peekable a, Peekable b) => Peekable (a, b) where
  peek = typeChecked "table" istable $ \idx ->
    (,) <$> nthValue idx 1 <*> nthValue idx 2

instance (Peekable a, Peekable b, Peekable c) =>
         Peekable (a, b, c)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3

instance (Peekable a, Peekable b, Peekable c, Peekable d) =>
         Peekable (a, b, c, d)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
          <*> nthValue idx 4

instance (Peekable a, Peekable b, Peekable c,
          Peekable d, Peekable e) =>
         Peekable (a, b, c, d, e)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
           <*> nthValue idx 4 <*> nthValue idx 5

instance (Peekable a, Peekable b, Peekable c,
          Peekable d, Peekable e, Peekable f) =>
         Peekable (a, b, c, d, e, f)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
            <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6


instance (Peekable a, Peekable b, Peekable c, Peekable d,
          Peekable e, Peekable f, Peekable g) =>
         Peekable (a, b, c, d, e, f, g)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
             <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6
             <*> nthValue idx 7

instance (Peekable a, Peekable b, Peekable c, Peekable d,
          Peekable e, Peekable f, Peekable g, Peekable h) =>
         Peekable (a, b, c, d, e, f, g, h)
 where
  peek = typeChecked "table" istable $ \idx ->
    (,,,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
              <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6
              <*> nthValue idx 7 <*> nthValue idx 8

-- | Helper function to get the nth table value
nthValue :: (PeekError e, Peekable a)
         => StackIndex -> Lua.Integer -> LuaE e a
nthValue idx n = do
  rawgeti idx n
  peek top `Catch.finally` pop 1
