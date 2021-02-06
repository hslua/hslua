{-|
Module      : Foreign.Lua
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Bindings, functions, and utilities enabling the integration of a Lua interpreter
into a haskell project.

Basic access to the Lua API is provided by '@Foreign.Lua.Core@'.
-}
module Foreign.Lua (
  -- * Core
    module Foreign.Lua.Core
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
  , newCFunction
  , freeCFunction
  , pushHaskellFunction
  , registerHaskellFunction
  -- * Utility functions and types
  , run
  , run'
  , runEither
  , runWith
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

import Foreign.Lua.Core
import Foreign.Lua.FunctionCalling
import Foreign.Lua.Module
import Foreign.Lua.Types
import Foreign.Lua.Userdata ( pushAny, peekAny )
import Foreign.Lua.Util
