{-# LANGUAGE CPP #-}
{-|
Module      : Lua.Warn
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Simpler interface to the Lua warnings system.
-}
module Lua.Warn
  ( hsluaL_setwarnf
  ) where

import Lua.Types (State (State))

-- | Sets a warning function. This is a simplified version of
-- 'Lua.Primary.lua_setwarnf'. The function at the top of the stack is
-- set as the "warning hook", i.e., it is called with the concatenated
-- warning components as the single argument.
--
-- The hook function is popped of the stack.
--
-- The control messages @\@on@ and @\@off@ are still supported; as with
-- the default warning function, these commands can switch error
-- reporting to stderr on and off. The given Haskell function will be
-- called in either case, even when the error is not written to stderr.
foreign import ccall "hslwarn.c hsluaL_setwarnf"
  hsluaL_setwarnf :: State -> IO ()
