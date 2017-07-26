{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017 Albert Krewinkel

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
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Bindings, functions, and utilities enabling the integration of a lua interpreter
into a haskell project.
-}
module Foreign.Lua
  ( Lua (..)
  , luaState
  , runLuaWith
  , liftIO
  -- * Receiving values from Lua stack (Lua → Haskell)
  , FromLuaStack (..)
  , peekEither
  , pairsFromTable
  -- * Pushing values to Lua stack (Haskell → Lua)
  , ToLuaStack (..)
  -- * Calling Function
  , LuaCFunction
  , luaimport
  , callfunc
  , newcfunction
  , pushhsfunction
  , pushrawhsfunction
  , registerhsfunction
  , registerrawhsfunction
  , mkWrapper
  -- * Utility functions
  , runLua
  , runLuaEither
  , getglobal'
  -- * API
  , module Foreign.Lua.Api
  , module Foreign.Lua.Api.Types
  -- * Error handling in hslua
  -- | We are trying to keep error handling on the haskell side very simple and
  -- intuitive. However, when combined with error handling on the Lua side, it
  -- get's tricky: We can call Haskell from Lua which calls Lua again etc. At
  -- each language boundary we should check for errors and propagate them
  -- properly to the next level in stack. Hslua does this for you when returning
  -- from Lua to Haskell, but care must be taken when passing errors back into
  -- Lua.
  --
  -- Let's say we have this call stack: (stack grows upwards)
  --
  -- > Haskell function
  -- > Lua function
  -- > Haskell program
  --
  -- and we want to report an error in the top-most Haskell function. We can't
  -- use @lua_error@ from the Lua C API, because it uses @longjmp@, which means
  -- it skips layers of abstractions, including the Haskell RTS. There's no way
  -- to prevent this @longjmp@. @lua_pcall@ sets the jump target, but even with
  -- @lua_pcall@ it's not safe. Consider this call stack:
  --
  -- > Haskell function which calls lua_error
  -- > Lua function, uses pcall
  -- > Haskell program
  --
  -- This program jumps to Lua function, skipping Haskell RTS code that would run
  -- before Haskell function returns. For this reason we can use
  -- @lua_pcall@ (@'pcall'@) only for catching errors from Lua, and even in that case
  -- we need to make sure there are no Haskell calls between error-throwing Lua
  -- call and our @'pcall'@ call.
  --
  -- To be able to catch errors from Haskell functions in Lua, we need to find a
  -- convention. Currently hslua does this: @'lerror'@ has same type as Lua's
  -- @lua_error@, but instead of calling real @lua_error@, it's returning two
  -- values: A special value @_HASKELLERR@ and error message as a string.
  --
  -- Using this, we can write a function to catch errors from Haskell like this:
  --
  -- > function catch_haskell(ret, err_msg)
  -- >     if ret == _HASKELLERR then
  -- >       print("Error caught from Haskell land: " .. err_msg)
  -- >       return
  -- >     end
  -- >     return ret
  -- > end
  --
  -- (`_HASKELLERR` is created by `newstate`)
  --
  -- (Type errors in Haskell functions are also handled using this convention.
  -- E.g., if you pass a Lua value with the wrong type to a Haskell function,
  -- the error will be reported in this way)
  --
  -- At this point our call stack is like this:
  --
  -- > Lua function (Haskell function returned with error, which we caught)
  -- > Haskell program
  --
  -- If we further want to propagate the error message to Haskell program, we we
  -- can just use standard @error@ function and use @'pcall'@ in Haskell side.
  -- Note that if we use @error@ on the Lua side and forget to use `pcall` in
  -- the calling Haskell function, we would be starting to skip layers of
  -- abstractions and would get a segfault in the best case. That's why hslua
  -- wraps all API functions that can potentially fail in custom C functions.
  -- Those functions behave idential to the functions they wrap, but catch all
  -- errors and return error codes instead. Using @error@ within Lua should
  -- hence be safe.
  --
  -- However, the raw C API bindings in @'Foreign.Lua.RawBindings'@ don't
  -- provide these guarantees. Even an apparently harmless operations like
  -- accessing a field via @'lua_getfield'@ can call a meta method and trigger a
  -- @longjmp@, causing the host program to crash.
  --
  -- The @'pcall'@ function is not wrapped in additional C code but still safe.
  -- The reason it's safe is because the @lua_pcall@ C function is calling the
  -- Lua function using Lua C API, and when the called Lua function calls
  -- @error@ it @longjmp@s to @lua_pcall@ C function, without skipping any
  -- layers of abstraction. @lua_pcall@ then returns to Haskell.
  --
  -- NOTE: If you're loading a hslua program compiled to a dynamic library from
  -- a Lua program, you need to define @_HASKELLERR = {}@ manually, after
  -- creating the Lua state.
  , LuaException (..)
  , catchLuaError
  , throwLuaError
  , tryLua
  ) where

import Prelude hiding (compare, concat)

import Foreign.Lua.Api
import Foreign.Lua.Api.Types
import Foreign.Lua.FunctionCalling
import Foreign.Lua.Types
import Foreign.Lua.Util

