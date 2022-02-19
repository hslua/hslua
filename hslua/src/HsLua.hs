{-|
Module      : HsLua
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Functions and utilities enabling the seamless integration of a Lua
interpreter into a Haskell project.

This module combines and re-exports the functionality of the HsLua
framework. Basic access to the Lua API is provided by @'Lua.Core'@ from
Hackage package /lua/.
-}
module HsLua (
    -- * Core functionality
    module HsLua.Core
    -- * Marshalling
  , module HsLua.Marshalling
    -- * Module, data, and function packaging
  , module HsLua.ObjectOrientation
  , module HsLua.Packaging
    -- * Type classes
  , module HsLua.Class.Exposable
  , module HsLua.Class.Invokable
  , module HsLua.Class.Peekable
  , module HsLua.Class.Pushable
    -- * Marshal to and from JSON-like structures
  , module HsLua.Aeson
    -- * Utility functions
  , getglobal'
  , setglobal'
  , module HsLua.Class.Util
  ) where

import Prelude hiding (compare, concat)

import HsLua.Aeson
import HsLua.Core
import HsLua.Class.Exposable
import HsLua.Class.Invokable
import HsLua.Class.Peekable
import HsLua.Class.Pushable hiding (pushList)
import HsLua.Class.Util
import HsLua.Packaging
import HsLua.Marshalling
import HsLua.ObjectOrientation
import HsLua.Util
