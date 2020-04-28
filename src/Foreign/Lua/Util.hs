{-|
Module      : Foreign.Lua.Util
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

HsLua utility functions.
-}
module Foreign.Lua.Util
  ( getglobal'
  , setglobal'
  , run
  , run'
  , runEither
  , raiseError
  , Optional (Optional, fromOptional)
    -- * Default error handling
  , runWith
    -- * getting values
  , peekEither
  , peekRead
  , popValue
  ) where

import Control.Exception (bracket, try)
import Data.List (groupBy)
import Foreign.Lua.Core (Lua, NumResults, StackIndex)
import Foreign.Lua.Types (Peekable, Pushable)
import Text.Read (readMaybe)

import qualified Control.Monad.Catch as Catch
import qualified Foreign.Lua.Core as Lua
import qualified Foreign.Lua.Types as Lua

-- | Run Lua computation using the default HsLua state as starting point.
-- Exceptions are masked, thus avoiding some issues when using multiple threads.
-- All exceptions are passed through; error handling is the responsibility of
-- the caller.
run :: Lua a -> IO a
run = (Lua.newstate `bracket` Lua.close) . flip runWith . Catch.mask_

-- | Run Lua computation using the default HsLua state as starting point.
-- Conversion from Lua errors to Haskell exceptions can be controlled through
-- @'Lua.ErrorConversion'@.
run' :: Lua.ErrorConversion -> Lua a -> IO a
run' ec = (Lua.newstate `bracket` Lua.close) .
  flip (Lua.runWithConverter ec) . Catch.mask_

-- | Run the given Lua computation; exceptions raised in haskell code are
-- caught, but other exceptions (user exceptions raised in haskell, unchecked
-- type errors, etc.) are passed through.
runEither :: Catch.Exception e => Lua a -> IO (Either e a)
runEither = try . run

-- | Run Lua computation with the given Lua state and the default
-- error-to-exception converter. Exception handling is left to
-- the caller.
runWith :: Lua.State -> Lua a -> IO a
runWith = Lua.runWithConverter defaultErrorConversion

-- | Conversions between Lua errors and Haskell exceptions; only deals with
-- @'Lua.Exception'@s.
defaultErrorConversion :: Lua.ErrorConversion
defaultErrorConversion = Lua.ErrorConversion
  { Lua.errorToException = Lua.throwTopMessageWithState
  , Lua.addContextToException = Lua.withExceptionMessage . (++)
  , Lua.alternative = \x y -> Lua.try x >>= \case
      Left _   -> y
      Right x' -> return x'
  , Lua.exceptionToError = (`Lua.catchException` \ (Lua.Exception msg) ->
                            raiseError msg)
  }

-- | Like @getglobal@, but knows about packages and nested tables. E.g.
--
-- > getglobal' "math.sin"
--
-- will return the function @sin@ in package @math@.
getglobal' :: String -> Lua ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: String -> Lua ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      Lua.setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      Lua.pushvalue (Lua.nthFromTop 2)
      Lua.setfield (Lua.nthFromTop 2) lastField
      Lua.pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: String -> [String]
splitdot = filter (/= ".") . groupBy (\a b -> a /= '.' && b /= '.')

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: [String] -> Lua ()
getnested [] = return ()
getnested (x:xs) = do
  Lua.getglobal x
  mapM_ (\a -> Lua.getfield Lua.stackTop a *> Lua.remove (Lua.nthFromTop 2)) xs

-- | Raise a Lua error, using the given value as the error object.
raiseError :: Pushable a => a -> Lua NumResults
raiseError e = do
  Lua.push e
  Lua.error
{-# INLINABLE raiseError #-}

-- | Newtype wrapper intended to be used for optional Lua values. Nesting this
-- type is strongly discouraged as missing values on inner levels are
-- indistinguishable from missing values on an outer level; wrong values
-- would be the likely result.
newtype Optional a = Optional { fromOptional :: Maybe a }

instance Peekable a => Peekable (Optional a) where
  peek idx = do
    noValue <- Lua.isnoneornil idx
    if noValue
      then return $ Optional Nothing
      else Optional . Just <$> Lua.peek idx

instance Pushable a => Pushable (Optional a) where
  push (Optional Nothing)  = Lua.pushnil
  push (Optional (Just x)) = Lua.push x


--
-- Getting Values
--

-- | Get a value by retrieving a String from Lua, then using @'readMaybe'@ to
-- convert the String into a Haskell value.
peekRead :: Read a => StackIndex -> Lua a
peekRead idx = do
  s <- Lua.peek idx
  case readMaybe s of
    Just x -> return x
    Nothing -> Lua.throwException ("Could not read: " ++ s)

-- | Try to convert the value at the given stack index to a Haskell value.
-- Returns @Left@ with an error message on failure.
--
-- WARNING: this is not save to use with custom error handling!
peekEither :: Peekable a => StackIndex -> Lua (Either String a)
peekEither idx = either (Left . Lua.exceptionMessage) Right <$>
                 Lua.try (Lua.peek idx)

-- | Get, then pop the value at the top of the stack. The pop operation is
-- executed even if the retrieval operation failed.
popValue :: Peekable a => Lua a
popValue = Lua.peek Lua.stackTop `Catch.finally` Lua.pop 1
{-# INLINABLE popValue #-}
