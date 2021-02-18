{-|
Module      : Foreign.Lua.Raw.Constants
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
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

import Foreign.C (CInt (..))
import Foreign.Lua.Raw.Types

-- | Alias for C constant @LUA_MULTRET@. See
-- <https://www.lua.org/manual/5.3/#lua_call lua_call>.
foreign import capi unsafe "lua.h value LUA_MULTRET"
  multret :: NumResults

-- | Alias for C constant @LUA_REGISTRYINDEX@. See
-- <https://www.lua.org/manual/5.3/#3.5 Lua registry>.
foreign import capi unsafe "lua.h value LUA_REGISTRYINDEX"
  registryindex :: StackIndex

-- | Value signaling that no reference was created.
foreign import capi unsafe "lauxlib.h value LUA_REFNIL"
  refnil :: Int

-- | Value signaling that no reference was found.
foreign import capi unsafe "lauxlib.h value LUA_NOREF"
  noref :: Int
