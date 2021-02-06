{-|
Module      : Foreign.Lua.Core.RawBindings
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Haskell bindings to Lua C API functions.

This module was moved to @'Foreign.Lua.Raw.Functions'@. It now
merely exists for backwards compatibility and will be removed in
the future.
-}
module Foreign.Lua.Core.RawBindings
  ( module Foreign.Lua.Raw.Functions
  ) where

import Foreign.Lua.Raw.Functions
