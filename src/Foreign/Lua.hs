{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017-2018 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-|
Module      : Foreign.Lua
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
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
  , runEither
  , getglobal'
  , setglobal'
  , raiseError
  , Optional (Optional, fromOptional)
  -- ** retrieving values
  , popValue
  ) where

import Prelude hiding (compare, concat)

import Foreign.Lua.Core
import Foreign.Lua.FunctionCalling
import Foreign.Lua.Types
import Foreign.Lua.Userdata ( pushAny, peekAny )
import Foreign.Lua.Util

