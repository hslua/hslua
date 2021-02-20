{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Call
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to function call helpers.
-}
module Foreign.Lua.Raw.Call
  {-# DEPRECATED "Use Foreign.Lua.Call instead" #-}
  (module Foreign.Lua.Call)
where

import Foreign.Lua.Call
