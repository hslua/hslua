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
  -- * Core
    module HsLua.Core
  -- * Receiving values from Lua stack (Lua → Haskell)
  , Peekable (..)
  , peekEither
  , peekList
  , peekKeyValuePairs
  , peekRead
  , peekAny
  -- * Pushing values to Lua stack (Haskell → Lua)
  , Pushable (..)
  , pushList
  , pushAny
  -- * Calling Functions
  , PreCFunction
  , HaskellFunction
  , ToHaskellFunction (..)
  , toHaskellFunction
  , callFunc
  , pushHaskellFunction
  , registerHaskellFunction
  -- * Utility functions and types
  , getglobal'
  , setglobal'
  , raiseError
  , Optional (Optional, fromOptional)
  -- ** Retrieving values
  , popValue
  -- ** Modules
  , requirehs
  , preloadhs
  , create
  , addfield
  , addfunction
  ) where

import Prelude hiding (compare, concat)

import HsLua.Core
import HsLua.FunctionCalling
import HsLua.Module
import HsLua.Types
import HsLua.Userdata ( pushAny, peekAny )
import HsLua.Util
