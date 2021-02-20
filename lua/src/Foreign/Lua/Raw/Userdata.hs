{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Userdata
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Bindings to HsLua-specific functions used to push Haskell values
as userdata.
-}
module Foreign.Lua.Raw.Userdata
  {-# DEPRECATED "Use Foreign.Lua.Userdata instead" #-}
  (module Foreign.Lua.Userdata)
where

import Foreign.Lua.Userdata
