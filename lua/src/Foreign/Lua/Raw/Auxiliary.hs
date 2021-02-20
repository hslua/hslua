{-|
Module      : Foreign.Lua.Raw.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to functions and constants of the auxiliary library.

*DEPRECATED*: Use Foreign.Lua.Auxiliary instead.
-}
module Foreign.Lua.Raw.Auxiliary
  {-# DEPRECATED "Use Foreign.Lua.Auxiliary instead" #-}
  (module Foreign.Lua.Auxiliary)
where

import Foreign.Lua.Auxiliary
