{-|
Module      : HsLua.Core.Run
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Helper functions to run 'LuaE' computations.
-}
module HsLua.Core.Run
  ( run
  , runEither
  , runWith
    -- * GCManaged state
  , GCManagedState
  , newGCManagedState
  , closeGCManagedState
  , withGCManagedState
  ) where

import Control.Exception (bracket, try)
import Control.Monad ((<$!>))
import Foreign.ForeignPtr
  (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import HsLua.Core.Types (LuaE, runWith)
import Lua.Primary (lua_close_ptr)
import Lua (State (..))

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

-- | Wrapper of a Lua state whose lifetime is managed by the Haskell
-- garbage collector and has a finalizer attached. This means that the
-- state does not have to be closed explicitly, but will be closed
-- automatically when the value is garbage collected in Haskell.
newtype GCManagedState = GCManagedState (ForeignPtr ())

-- | Creates a new Lua state that is under the control of the Haskell
-- garbage collector.
newGCManagedState :: IO GCManagedState
newGCManagedState = do
  (State lptr) <- Lua.newstate
  GCManagedState <$!> newForeignPtr lua_close_ptr lptr

-- | Closes the Lua state and runs all finalizers associated with it.
-- The state _may not_ be used after it has been closed.
closeGCManagedState :: GCManagedState -> IO ()
closeGCManagedState (GCManagedState fptr) = finalizeForeignPtr fptr

-- | Runs a Lua action with a state that's managed by GC.
withGCManagedState :: GCManagedState
                   -> LuaE e a
                   -> IO a
withGCManagedState (GCManagedState fptr) action =
  withForeignPtr fptr $ \lptr ->
    runWith (State lptr) action
