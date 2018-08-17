{-|
Module      : Foreign.Lua.Core
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Core Lua API. This module provides thin wrappers around the respective functions
of the Lua C API. C function which can throw an error are wrapped such that the
error is converted into an @'Exception'@. However, memory allocation errors are
not caught and will cause the host program to terminate.
-}
module Foreign.Lua.Core (
  -- * Lua Computations
    Lua (..)
  , runWith
  , liftIO
  , state
  -- * Lua API types
  , CFunction
  , Lua.Integer (..)
  , Lua.Number (..)
  -- ** Stack index
  , StackIndex (..)
  , nthFromBottom
  , nthFromTop
  , stackTop
  , stackBottom
  -- ** Number of arguments and return values
  , NumArgs (..)
  , NumResults (..)
  -- * Lua API
  -- ** Constants and pseudo-indices
  , multret
  , registryindex
  , upvalueindex
  -- ** State manipulation
  , Lua.State (..)
  , newstate
  , close
  -- ** Basic stack manipulation
  , absindex
  , gettop
  , settop
  , pushvalue
  , copy
  , insert
  , pop
  , remove
  , replace
  , checkstack
  -- ** types and type checks
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , ltype
  , typename
  , isboolean
  , iscfunction
  , isfunction
  , islightuserdata
  , isnil
  , isnone
  , isnoneornil
  , isnumber
  , isstring
  , istable
  , isthread
  , isuserdata
  -- ** access functions (stack → Haskell)
  , toboolean
  , tocfunction
  , tointeger
  , tonumber
  , topointer
  , tostring
  , tothread
  , touserdata
  , rawlen
  -- ** Comparison and arithmetic functions
  , RelationalOperator (..)
  , fromRelationalOperator
  , compare
  , equal
  , lessthan
  , rawequal
  -- ** push functions (Haskell → stack)
  , pushboolean
  , pushcfunction
  , pushcclosure
  , pushinteger
  , pushlightuserdata
  , pushnil
  , pushnumber
  , pushstring
  , pushthread
  -- ** get functions (Lua → stack)
  , getglobal
  , gettable
  , getfield
  , rawget
  , rawgeti
  , createtable
  , newtable
  , newuserdata
  , getmetatable
  -- ** set functions (stack → Lua)
  , setglobal
  , settable
  , setfield
  , rawset
  , rawseti
  , setmetatable
  -- ** load and call functions (load and run Lua code)
  , call
  , pcall
  , load
  , loadbuffer
  , loadfile
  , loadstring
  -- ** Coroutine functions
  , Status (..)
  , toStatus
  , status
  -- ** garbage-collection function and options
  , GCCONTROL (..)
  , gc
  -- ** miscellaneous and helper functions
  , next
  , error
  , concat
  , register
  -- * loading libraries
  , openbase
  , opendebug
  , openio
  , openlibs
  , openmath
  , openpackage
  , openos
  , openstring
  , opentable
  -- * Auxiliary library
  , dostring
  , dofile
  , newmetatable
  , tostring'
  -- ** References
  , Reference (..)
  , ref
  , getref
  , unref
  , fromReference
  , toReference
  , noref
  , refnil
  -- * Error handling
  -- | We are trying to keep error handling on the haskell side very simple
  -- and intuitive. However, when combined with error handling on the Lua
  -- side, it get's tricky: We can call Haskell from Lua which calls Lua again
  -- etc. At each language boundary we have to check for errors and propagate
  -- them properly to the next level in stack. Hslua does this for you, both
  -- when returning from Lua to Haskell, and when calling from Haskell into
  -- Lua. However, some minor care must be taken in these cases when passing
  -- errors back into Lua.
  --
  -- ** Background
  -- | Let's say we have this call stack: (stack grows upwards)
  --
  -- > Haskell function
  -- > Lua function
  -- > Haskell program
  --
  -- and we want to report an error in the top-most Haskell function. We can't
  -- use @lua_error@ from the Lua C API, because it uses @longjmp@, which
  -- means it skips layers of abstractions, including the Haskell RTS. There's
  -- no way to prevent this @longjmp@. @lua_pcall@ sets the jump target, but
  -- even with @lua_pcall@ it's not safe. Consider this call stack:
  --
  -- > Haskell function which calls lua_error
  -- > Lua function, uses pcall
  -- > Haskell program
  --
  -- This program jumps to Lua function, skipping Haskell RTS code that would
  -- run before Haskell function returns. For this reason we can use
  -- @lua_pcall@ (@'pcall'@) only for catching errors from Lua, and even in
  -- that case we need to make sure there are no Haskell calls between
  -- error-throwing Lua call and our @'pcall'@ call.
  --
  -- To be able to catch exceptions from Haskell functions in Lua, we need to
  -- find a convention. Currently hslua does this: @'error'@ has the same type
  -- as Lua's @lua_error@, but instead of calling @lua_error@, it returns two
  -- values: A special error value and an error message as a string. Hslua
  -- exceptions are caught and converted to Lua errors via this function.
  --
  -- These internals should stay hidden most of the time, as all Haskell
  -- functions are wrapped in such a way, that error values are transformed
  -- into Lua errors behind the scenes.
  --
  -- At this point our call stack is like this:
  --
  -- > Lua function (Haskell function returned with error, which we caught)
  -- > Haskell program
  --
  -- If we want to further propagate the error message to the Haskell program,
  -- then we can just use Lua's standard @error@ function and use @'pcall'@ on
  -- the Haskell side. Note that if we use @error@ on the Lua side and forget
  -- to use `pcall` in the calling Haskell function, we would be starting to
  -- skip layers of abstractions and would get a segfault in the best case.
  -- That's why hslua wraps all API functions that can potentially fail in
  -- custom C functions. Those functions behave idential to the functions they
  -- wrap, but catch all errors and return error codes instead. This comes
  -- with a serious performance penalty, but using @error@ within Lua should
  -- be safe.
  --
  -- The @'pcall'@ function is not wrapped in additional C code but still safe.
  -- The reason it's safe is because the @lua_pcall@ C function is calling the
  -- Lua function using Lua C API, and when the called Lua function calls
  -- @error@ it @longjmp@s to @lua_pcall@ C function, without skipping any
  -- layers of abstraction. @lua_pcall@ then returns to Haskell.
  --
  -- NOTE: If you're loading a hslua program compiled to a dynamic library from
  -- a Lua program, you need to set @HSLUA_ERR@ in the registry to any unique
  -- value manually, after creating the Lua state.
  , Exception (..)
  , throwException
  , catchException
  , modifyException
  , try
  , throwTopMessage
  ) where

import Prelude hiding (EQ, LT, compare, concat, error)

import Foreign.Lua.Core.Auxiliary
import Foreign.Lua.Core.Constants
import Foreign.Lua.Core.Error
import Foreign.Lua.Core.Functions
import Foreign.Lua.Core.Types as Lua
