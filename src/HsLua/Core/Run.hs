{-|
Module      : HsLua.Core.Run
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Helper functions to run 'Lua' computations.
-}
module HsLua.Core.Run
  ( run
  , run'
  , runEither
  , runWith
  , defaultErrorConversion
  ) where

import Control.Exception (bracket, try)
import HsLua.Core.Types (Lua)

import qualified Control.Monad.Catch as Catch
import qualified HsLua.Core.Auxiliary as Lua
import qualified HsLua.Core.Error as Lua
import qualified HsLua.Core.Functions as Lua
import qualified HsLua.Core.Types as Lua
import qualified HsLua.Core.Utf8 as Utf8

-- | Run Lua computation using the default HsLua state as starting
-- point. Exceptions are masked, thus avoiding some issues when using
-- multiple threads. All exceptions are passed through; error handling
-- is the responsibility of the caller.
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
  , Lua.exceptionToError = (`Lua.catchException`
                             \(Lua.Exception msg) -> do
                               Lua.pushstring $ Utf8.fromString msg
                               Lua.error)
  }
