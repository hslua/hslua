{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Foreign.Lua.Raw.Types
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

The core Lua types, including mappings of Lua types to Haskell.
-}
module Foreign.Lua.Raw.Types
  {-# DEPRECATED "Use Foreign.Lua.Auxiliary instead" #-}
  (module Foreign.Lua.Types)
where

import Foreign.Lua.Types
