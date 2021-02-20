{-|
Module      : Foreign.Lua.Raw.Error
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

Lua exceptions and exception handling.
-}
module Foreign.Lua.Raw.Error
  {-# DEPRECATED "Use Foreign.Lua.Error instead" #-}
  (module Foreign.Lua.Error)
where

import Foreign.Lua.Error
