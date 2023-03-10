{-|
Module      : Lua.Ersatz.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to ersatz functions of the auxiliary library.
-}
module Lua.Ersatz.Auxiliary
  ( -- * Auxiliary Library
    hsluaL_newstate
  , hsluaL_requiref
  , hsluaL_tolstring
  ) where

import Foreign.C (CChar, CInt (CInt), CSize)
import Lua.Types (CFunction, StackIndex, StatusCode)
import Foreign.Ptr (Ptr)
import qualified Lua.Types as Lua

-- | Creates a new Lua state and set extra registry values for error
-- bookkeeping.
foreign import ccall unsafe "hslauxlib.h hsluaL_newstate"
  hsluaL_newstate :: IO Lua.State

-- | If @modname@ is not already present in @package.loaded@. calls
-- function @openf@ with string @modname@ as an argument and sets the
-- call result in @package.loaded[modname]@, as if that function has
-- been called through
-- <https://www.lua.org/manual/5.4/manual.html#pdf-require require>.
--
-- If @glb@ is true, also stores the module into global @modname@.
--
-- Leaves a copy of the module on the stack.
foreign import ccall unsafe "hslauxlib.c hsluaL_requiref"
  hsluaL_requiref :: Lua.State
                  -> Ptr CChar   -- ^ modname
                  -> CFunction   -- ^ openf
                  -> Lua.LuaBool -- ^ glb
                  -> Ptr StatusCode
                  -> IO ()

-- | Converts object to string, respecting any metamethods; returns
-- @NULL@ if an error occurs.
foreign import ccall safe "hslauxlib.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)
