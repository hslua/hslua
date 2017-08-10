{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Foreign.Lua.Api.SafeBindings
Copyright   : © 2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Haskell bindings to safe, @longjmp@ catching wrappers of Lua C API functions.

Functions in this module follow a custom protocol. If an error happens (i.e., if
the lua status is not 0), the status code is returned as a negative number.
Results of a successful execution of the wrapped function are returned as
non-negative numbers. This is possible because all (supported) lua API functions
are either safe and don't need wrapping, or return @void@, @size_t@ or a
non-negative @int@.
-}
module Foreign.Lua.Api.SafeBindings where

import Foreign.C
import Foreign.Lua.Api.Types
import Foreign.Ptr

#include "safer-api.h"

#if LUA_VERSION_NUM >= 502
-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_compare \
-- @lua_compare@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_compare"
  hslua_compare :: LuaState -> StackIndex -> StackIndex -> CInt -> IO CInt
#endif

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_concat \
-- @lua_concat@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_concat"
  hslua_concat :: LuaState -> CInt -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_getfield \
-- @lua_getfield@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_getfield"
  hslua_getfield :: LuaState -> StackIndex -> Ptr CChar -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_getglobal \
-- @lua_getglobal@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_getglobal"
  hslua_getglobal :: LuaState -> Ptr CChar -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_gettable \
-- @lua_gettable@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_gettable"
  hslua_gettable :: LuaState -> StackIndex -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_next \
-- @lua_next@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_next"
  hslua_next :: LuaState -> StackIndex -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_setfield \
-- @lua_setfield@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_setfield"
  hslua_setfield :: LuaState -> StackIndex -> Ptr CChar -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_setglobal \
-- @lua_setglobal@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_setglobal"
  hslua_setglobal :: LuaState -> Ptr CChar -> IO CInt

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_settable \
-- @lua_settable@> which catches any @longjmp@s.
foreign import ccall "safer-api.h hslua_settable"
  hslua_settable :: LuaState -> StackIndex -> IO CInt
