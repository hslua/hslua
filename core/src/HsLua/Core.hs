{-|
Module      : HsLua.Core
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Core Lua API. This module provides thin wrappers around the respective
functions of the Lua C API. C functions which can throw an error are
wrapped such that the error is converted into an @'Exception'@. However,
memory allocation errors are not caught and will cause the host program
to terminate.
-}
module HsLua.Core
  ( -- * Run Lua computations
    run
  , run'
  , runWith
  , runEither
  -- * Lua Computations
  , Lua (..)
  , runWithConverter
  , unsafeRunWith
  , liftIO
  , state
  , LuaEnvironment (..)
  , ErrorConversion (..)
  , errorConversion
  , unsafeErrorConversion
  -- * Lua API types
  , CFunction
  , Lua.Integer (..)
  , Lua.Number (..)
  -- ** Stack index
  , StackIndex (..)
  , nthTop
  , nthBottom
  , nth
  , top
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
  , isinteger
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
  , pushglobaltable
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
  , getmetafield
  , getmetatable'
  , getsubtable
  , newmetatable
  , tostring'
  , traceback
  -- ** References
  , Reference (..)
  , ref
  , getref
  , unref
  , fromReference
  , toReference
  , noref
  , refnil
  -- ** Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
  -- * Error handling
  , Exception (..)
  , throwException
  , catchException
  , withExceptionMessage
  , try
  , throwMessage
  , errorMessage
  , throwErrorAsException
  , throwTopMessage
  , throwTopMessageWithState
  ) where

import Prelude hiding (EQ, LT, compare, concat, error)

import HsLua.Core.Auxiliary
import HsLua.Core.Error
import HsLua.Core.Functions
import HsLua.Core.Run
import HsLua.Core.Types as Lua
