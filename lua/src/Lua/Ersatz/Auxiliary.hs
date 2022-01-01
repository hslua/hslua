{-|
Module      : Lua.Ersatz.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to ersatz functions of the auxiliary library.
-}
module Lua.Ersatz.Auxiliary
  ( -- * Auxiliary Library
    hsluaL_newstate
  , hsluaL_tolstring
  ) where

import Foreign.C (CChar, CInt (CInt), CSize)
import Lua.Types (StackIndex)
import Foreign.Ptr (Ptr)
import qualified Lua.Types as Lua

-- | Creates a new Lua state and set extra registry values for error
-- bookkeeping.
foreign import ccall unsafe "hslauxlib.h hsluaL_newstate"
  hsluaL_newstate :: IO Lua.State

-- | Converts object to string, respecting any metamethods; returns
-- @NULL@ if an error occurs.
foreign import ccall safe "hslauxlib.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)
