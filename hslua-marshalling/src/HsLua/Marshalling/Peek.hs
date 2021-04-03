{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-|
Module      : HsLua.Marshalling.Peek
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Types for unmarshalling of values from Lua.
-}
module HsLua.Marshalling.Peek
  ( Peeker
  , Result (..)
  , isFailure
  , failure
  , force
  , retrieving
  , resultToEither
  , toPeeker
  -- * Lua peek monad
  , Peek (..)
  , runPeek
  , withContext
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<$!>))
import Data.ByteString (ByteString)
import Data.List (intercalate)
import HsLua.Core as Lua
#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (..))
#endif
import qualified HsLua.Core.Utf8 as Utf8

-- | Record to keep track of failure contexts while retrieving objects
-- from the Lua stack.
data Result a
  = Success a
  | Failure ByteString [Name]
  deriving (Show, Eq, Functor)

instance Applicative Result where
  pure = Success
  Success f         <*> s = fmap f s
  Failure msg stack <*> _         = Failure msg stack

instance Monad Result where
  Failure msg stack >>= _ = Failure msg stack
  Success x         >>= f = f x

instance Alternative Result where
  empty = Failure "empty" []
  x <|> y = case x of
    Failure {} -> y
    _          -> x

--
-- Peek
--

-- | Lua operation with an additional failure mode that can stack errors
-- from different contexts; errors are not based on exceptions).
newtype Peek e a = Peek (LuaE e (Result a))
  deriving (Functor)

-- | The inverse of Peek.
runPeek :: Peek e a -> LuaE e (Result a)
runPeek (Peek x) = x

instance Applicative (Peek e) where
  pure = Peek . return . pure
  {-# INLINE pure #-}

  Peek f <*> x = Peek $! f >>= \case
    Failure msg stack -> return $ Failure msg stack
    Success f'        -> fmap f' <$!> runPeek x
  {-# INLINEABLE (<*>) #-}

  m *> k = m >>= const k
  {-# INLINE (*>) #-}

instance Monad (Peek e) where
  Peek m >>= k = Peek $
    m >>= \case
      Failure msg stack -> return $ Failure msg stack
      Success x         -> runPeek (k x)

instance Alternative (Peek e) where
  a <|> b = Peek $ runPeek a >>= \case
    Failure {} -> runPeek b
    Success ra -> return (pure ra)
  empty = Peek . return $ failure "empty"

instance MonadFail (Peek e) where
  fail = Peek . return . failure . Utf8.fromString

-- | Transform the result using the given function.
withContext :: Name -> Peek e a -> Peek e a
withContext ctx = Peek . retrieving ctx . runPeek


-- | Returns 'True' iff the peek result is a Failure.
isFailure :: Result a -> Bool
isFailure Failure {} = True
isFailure _          = False

-- | Combines the peek failure components into a reportable string.
formatPeekFailure :: ByteString -> [Name] -> String
formatPeekFailure msg stack =
  intercalate "\n\twhile retrieving " $
  map Utf8.toString (msg : map fromName (reverse stack))

-- | Function to retrieve a value from Lua's stack.
type Peeker e a = StackIndex -> LuaE e (Result a)

-- | Create a peek failure record from an error message.
failure :: ByteString -> Result a
failure msg = Failure msg []

-- | Add a message to the peek traceback stack.
addFailureContext :: Name -> Result a -> Result a
addFailureContext name = \case
  Failure msg stack -> Failure msg (name : stack)
  x -> x

-- | Add context information to the peek traceback stack.
retrieving :: Name
           -> LuaE e (Result a)
           -> LuaE e (Result a)
retrieving msg = fmap (addFailureContext msg)

-- | Force creation of an unwrapped result, throwing an exception if
-- that's not possible.
force :: LuaError e => Result a -> LuaE e a
force = \case
  Success x -> return x
  Failure msg stack -> failLua $ formatPeekFailure msg stack

-- | Converts a Result into an Either, where @Left@ holds the reportable
-- string in case of an failure.
resultToEither :: Result a -> Either String a
resultToEither = \case
  Failure msg stack -> Left $ formatPeekFailure msg stack
  Success x         -> Right x

-- | Converts an old peek funtion to a 'Peeker'.
toPeeker :: LuaError e
         => (StackIndex -> LuaE e a)
         -> Peeker e a
toPeeker op idx = try (op idx) >>= \case
  Left err  -> return $! failure $ Utf8.fromString (show err)
  Right res -> return $! Success res
