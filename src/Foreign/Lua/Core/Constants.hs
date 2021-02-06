{-|
Module      : Foreign.Lua.Core.Constants
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Lua constants. This module was moved to
@'Foreign.Lua.Raw.Constants'@. It now merely exists for backwards
compatibility and will be removed in the future.
-}
module Foreign.Lua.Core.Constants
  ( multret
  , registryindex
  , refnil
  , noref
  ) where

import Foreign.Lua.Raw.Constants
