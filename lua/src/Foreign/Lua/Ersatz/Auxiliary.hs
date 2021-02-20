{-|
Module      : Foreign.Lua.Ersatz.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to ersatz functions of the auxiliary library.
-}
module Foreign.Lua.Ersatz.Auxiliary
  ( -- * Auxiliary Library
    hsluaL_newstate
  , hsluaL_tolstring
  ) where

import Foreign.C (CChar, CInt (CInt), CSize)
import Foreign.Lua.Types (StackIndex)
import Foreign.Ptr (Ptr)
import qualified Foreign.Lua.Types as Lua

foreign import ccall unsafe "hslauxlib.h hsluaL_newstate"
  hsluaL_newstate :: IO Lua.State

foreign import ccall safe "hslauxlib.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)
