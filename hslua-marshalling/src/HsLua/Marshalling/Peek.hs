{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Marshalling.Peek
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Types for unmarshalling of values from Lua.
-}
module HsLua.Marshalling.Peek
  ( Peeker
  , runPeeker
  , Result (..)
  , isFailure
  , failure
  , force
  , retrieving
  , resultToEither
  , toPeeker
  -- * Lua peek monad
  , Peek (..)
  , forcePeek
  , failPeek
  , liftLua
  , withContext
  , lastly
  , cleanup
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<$!>), (<=<))
import Data.ByteString (ByteString)
import Data.List (intercalate)
import HsLua.Core as Lua
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (..))
#endif
#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif
import qualified HsLua.Core.Utf8 as Utf8

-- | Record to keep track of failure contexts while retrieving objects
-- from the Lua stack.
data Result a
  = Success !a
  | Failure ByteString [Name]       -- ^ Error message and stack of contexts
  deriving (Show, Eq, Functor)

instance Applicative Result where
  pure = Success
  {-# INLINE pure #-}
  Success f         <*> s = f <$!> s
  Failure msg stack <*> _ = Failure msg stack
  {-# INLINE (<*>) #-}

instance Monad Result where
  Failure msg stack >>= _ = Failure msg stack
  Success x         >>= f = f x

instance Alternative Result where
  empty = Failure "empty" []
  {-# INLINE empty #-}
  x <|> y = case x of
    Failure {} -> y
    _          -> x
  {-# INLINE (<|>) #-}

--
-- Peek
--

-- | Lua operation with an additional failure mode that can stack errors
-- from different contexts; errors are not based on exceptions).
newtype Peek e a = Peek { runPeek :: LuaE e (Result a) }
  deriving (Functor)

-- | Converts a Peek action into a LuaE action, throwing an exception in
-- case of a peek failure.
forcePeek :: LuaError e => Peek e a -> LuaE e a
forcePeek = force <=< runPeek
{-# INLINE forcePeek #-}

-- | Fails the peek operation.
failPeek :: forall a e. ByteString -> Peek e a
failPeek = Peek . return . failure
{-# INLINE failPeek #-}

-- | Lifts a Lua operation into the Peek monad.
liftLua :: LuaE e a -> Peek e a
liftLua = Peek . fmap pure
{-# INLINE liftLua #-}

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
  {-# INLINE (>>=) #-}

instance Alternative (Peek e) where
  empty = Peek . return $ failure "empty"
  {-# INLINE empty #-}

  a <|> b = Peek $ runPeek a >>= \case
    Success ra -> return (pure ra)
    _          -> runPeek b
  {-# INLINE (<|>) #-}

instance MonadFail (Peek e) where
  fail = Peek . return . failure . Utf8.fromString
  {-# INLINABLE fail #-}

-- | Transform the result using the given function.
withContext :: Name -> Peek e a -> Peek e a
withContext ctx = Peek . fmap (addFailureContext ctx) . runPeek
{-# INLINABLE withContext #-}

-- | Runs the peek action and Lua action in sequence, even if the peek
-- action fails.
lastly :: Peek e a -> LuaE e b -> Peek e a
lastly p after = Peek $! runPeek p <* after
{-# INLINABLE lastly #-}

-- | Runs the peek action, resetting the stack top afterwards. This can
-- be used with peek actions that might otherwise leave elements on the
-- stack in case of a failure.
cleanup :: Peek e a -> Peek e a
cleanup p = Peek $ do
  oldtop <- gettop
  result <- runPeek p
  settop oldtop
  return result
{-# INLINABLE cleanup #-}

-- | Returns 'True' iff the peek result is a Failure.
isFailure :: Result a -> Bool
isFailure Failure {} = True
isFailure _          = False

-- | Combines the peek failure components into a reportable string.
formatPeekFailure :: ByteString -> [Name] -> String
formatPeekFailure msg stack =
  intercalate "\n\twhile " $
  map Utf8.toString (msg : map fromName (reverse stack))

-- | Function to retrieve a value from Lua's stack.
type Peeker e a = StackIndex -> Peek e a

-- | Runs the peeker function.
runPeeker :: Peeker e a -> StackIndex -> LuaE e (Result a)
runPeeker p = runPeek . p

-- | Create a peek failure record from an error message.
failure :: ByteString -> Result a
failure msg = Failure msg []

-- | Add a message to the peek traceback stack.
addFailureContext :: Name -> Result a -> Result a
addFailureContext name = \case
  Failure msg stack -> Failure msg (name : stack)
  x -> x
{-# INLINABLE addFailureContext #-}

-- | Add context information to the peek traceback stack.
retrieving :: Name
           -> Peek e a
           -> Peek e a
retrieving = withContext . ("retrieving " <>)
{-# INLINE retrieving #-}

-- | Force creation of an unwrapped result, throwing an exception if
-- that's not possible.
force :: LuaError e => Result a -> LuaE e a
force = \case
  Success x -> return x
  Failure msg stack -> failLua $ formatPeekFailure msg stack
{-# INLINABLE force #-}

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
toPeeker op idx = Peek $ try (op idx) >>= \case
  Left err  -> return $! failure $ Utf8.fromString (show err)
  Right res -> return $! Success res
