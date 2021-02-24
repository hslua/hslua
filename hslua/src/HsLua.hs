{-|
Module      : HsLua
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Functions and utilities enabling the seamless integration of a Lua
interpreter into a Haskell project.

Basic access to the Lua API is provided by @'Lua.Core'@ from
Hackage package /lua/.
-}
module HsLua (
    -- * Core functionality
    module HsLua.Core
    -- * Receiving values from Lua stack (Lua → Haskell)
  , peekAny
    -- * Pushing values to Lua stack (Haskell → Lua)
  , pushAny
    -- * Utility functions
  , getglobal'
  , setglobal'
    -- ** Modules
  , requirehs
  , preloadhs
  , create
  ) where

import Prelude hiding (compare, concat)

import HsLua.Core
import HsLua.Module
import HsLua.Userdata ( pushAny, peekAny )
import HsLua.Util
