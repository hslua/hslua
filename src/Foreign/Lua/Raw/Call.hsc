{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Call
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to function call helpers.
-}
module Foreign.Lua.Raw.Call
  ( hslua_call_hs_ptr
  ) where

import Foreign.Lua.Raw.Types (CFunction)

-- | Convert callable userdata at top of stack into a CFunction,
-- translating errors to Lua errors. Use with @'pushcclosure'@.
foreign import ccall safe "error-conversion.h &hslua_call_hs"
  hslua_call_hs_ptr :: CFunction
