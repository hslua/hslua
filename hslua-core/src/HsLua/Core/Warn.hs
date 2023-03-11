{-|
Module      : HsLua.Core.Warn
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Simpler interface to the Lua warnings system.

This module simplifies the process of setting a custom warn function.
-}
module HsLua.Core.Warn
  ( setwarnf'
  ) where

import Data.ByteString (ByteString)
import HsLua.Core.Closures (pushHaskellFunction)
import HsLua.Core.Error (LuaError)
import HsLua.Core.Primary (tostring)
import HsLua.Core.Types (LuaE, NumResults (..), liftLua, nthBottom)
import Lua.Warn (hsluaL_setwarnf)

-- | Sets a warning function. This is a simplified version of
-- 'lua_setwarnf'. The given function is called with the concatenated
-- warning components as the single argument.
--
-- Control messages are handled internally and are /not/ passed on the
-- warning hook. As with the default warning function, the control
-- messages @\@on@ and @\@off@ can switch error reporting to stderr on
-- and off. The given Haskell function will be called in either case,
-- even when the error is not written to stderr.
--
-- Wraps 'hsluaL_setwarnf'.
setwarnf' :: LuaError e
          => (ByteString -> LuaE e ())
          -> LuaE e ()
setwarnf' fn = do
  pushHaskellFunction $ do
    mbmsg <- tostring (nthBottom 1)
    case mbmsg of
      Nothing  -> pure (NumResults 0)  -- couldn't get warning msg; do nothing
      Just msg -> NumResults 0 <$ fn msg
  liftLua hsluaL_setwarnf
