{-|
Module      : HsLua.Core.Run
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Helper functions to run 'LuaE' computations.
-}
module HsLua.Core.Run
  ( run
  , runEither
  , runWith
  ) where

import Control.Exception (bracket, try)
import HsLua.Core.Types (LuaE, runWith)

import qualified Control.Monad.Catch as Catch
import qualified HsLua.Core.Auxiliary as Lua
import qualified HsLua.Core.Primary as Lua

-- | Run Lua computation using the default HsLua state as starting
-- point. Exceptions are masked, thus avoiding some issues when using
-- multiple threads. All exceptions are passed through; error handling
-- is the responsibility of the caller.
run :: LuaE e a -> IO a
run = (Lua.newstate `bracket` Lua.close) . flip runWith . Catch.mask_
{-# INLINABLE run #-}

-- | Run the given Lua computation; exceptions raised in Haskell code are
-- caught, but other exceptions (user exceptions raised in Haskell, unchecked
-- type errors, etc.) are passed through.
runEither :: Catch.Exception e => LuaE e a -> IO (Either e a)
runEither = try . run
{-# INLINABLE runEither #-}
