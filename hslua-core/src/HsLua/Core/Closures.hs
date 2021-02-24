{-|
Module      : HsLua.Core.Closures
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Expose Haskell functions as Lua closures.
-}
module HsLua.Core.Closures
  ( pushPreCFunction
  , pushHaskellFunction
  ) where

import HsLua.Core.Types
  ( Lua, PreCFunction, HaskellFunction, ErrorConversion (exceptionToError)
  , errorConversion, liftLua, runWithConverter )
import Lua.Call (hslua_pushhsfunction)

-- | Converts a pre C function to a Lua function and pushes it to the
-- stack.
--
-- Pre C functions collect parameters from the stack and return a @CInt@
-- that represents number of return values left on the stack.
-- See 'Lua.CFunction' for more info.
pushPreCFunction :: PreCFunction -> Lua ()
pushPreCFunction preCFn = liftLua $ \l ->
  hslua_pushhsfunction l preCFn

-- | Pushes Haskell function as a callable userdata. All values created
-- will be garbage collected. The function should behave similar to a
-- 'CFunction'.
--
-- Error conditions should be indicated by raising a catchable exception
-- or by returning the result of @'Lua.error'@.
--
-- Example:
--
-- > mod23 :: Lua NumResults
-- > mod23 = do
-- >   mn <- tointeger (nthBottom 1)
-- >   case mn of
-- >     Nothing -> pushstring "expected an integer" *> error
-- >     Just n  -> pushinteger (n `mod` 23)
-- > pushHaskellFunction mod23
-- > setglobal "mod23"
pushHaskellFunction :: HaskellFunction -> Lua ()
pushHaskellFunction fn = do
  errConv <- errorConversion
  let preCFn l = runWithConverter errConv l (exceptionToError errConv fn)
  pushPreCFunction preCFn
