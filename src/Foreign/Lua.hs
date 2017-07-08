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
  ( module Foreign.Lua.Api.Constants
  , module Foreign.Lua.Api.Types
  , module Foreign.Lua.Interop
  , module Foreign.Lua.Types
  , module Foreign.Lua.Util
  -- * Lua API functions
  , atpanic
  , call
  , checkstack
  , close
  , compare
  , concat
  , copy
  , cpcall
  , createtable
  , equal
  , getfield
  , getglobal
  , getmetatable
  , gettable
  , gettop
  , gc
  , insert
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
  , lerror
  , lessthan
  , loadfile
  , loadstring
  , ltype
  , newmetatable
  , newstate
  , newtable
  , newuserdata
  , next
  , objlen
  , openbase
  , opendebug
  , openio
  , openlibs
  , openmath
  , openpackage
  , openos
  , openstring
  , opentable
  , pcall
  , pop
  , pushboolean
  , pushcfunction
  , pushinteger
  , pushlightuserdata
  , pushnil
  , pushnumber
  , pushstring
  , pushthread
  , pushvalue
  , rawequal
  , rawget
  , rawgeti
  , rawlen
  , rawset
  , rawseti
  , ref
  , register
  , remove
  , replace
  , setfield
  , setglobal
  , setmetatable
  , settable
  , settop
  , status
  , strlen
  , toboolean
  , tocfunction
  , tointeger
  , tonumber
  , topointer
  , tostring
  , tothread
  , touserdata
  , typename
  , upvalueindex
  , unref
  ) where

import Prelude hiding (compare, concat)

import Foreign.Lua.Api
import Foreign.Lua.Api.Constants
import Foreign.Lua.Api.Types
import Foreign.Lua.Interop
import Foreign.Lua.Types
import Foreign.Lua.Util
