{-|
Module      : Foreign.Lua.Raw.Constants
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Lua constants
-}
module Foreign.Lua.Raw.Constants
  ( multret
  , registryindex
  , refnil
  , noref
  ) where

import Foreign.Lua.Raw.Types

#include "lua.h"
#include "lauxlib.h"

-- | Alias for C constant @LUA_MULTRET@. See
-- <https://www.lua.org/manual/5.3/#lua_call lua_call>.
multret :: NumResults
multret = NumResults $ #{const LUA_MULTRET}

-- | Alias for C constant @LUA_REGISTRYINDEX@. See
-- <https://www.lua.org/manual/5.3/#3.5 Lua registry>.
registryindex :: StackIndex
registryindex = StackIndex $ #{const LUA_REGISTRYINDEX}

-- | Value signaling that no reference was created.
refnil :: Int
refnil = #{const LUA_REFNIL}

-- | Value signaling that no reference was found.
noref :: Int
noref = #{const LUA_NOREF}
