{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to functions and constants of the auxiliary library.
-}
module Foreign.Lua.Auxiliary
  ( hsluaL_newstate
  , hsluaL_tolstring
  , luaL_getmetafield
  , luaL_getmetatable
  , luaL_loadbuffer
  , luaL_newmetatable
  , luaL_ref
  , luaL_testudata
  , luaL_traceback
  , luaL_unref
    -- * Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
    -- * References
  , Reference (..)
  , fromReference
  , toReference
  ) where

import Foreign.C (CChar, CInt (CInt), CSize (CSize), CString)
import Foreign.Lua.Types (StackIndex)
import Foreign.Ptr
import qualified Foreign.Lua.Types as Lua

#ifndef HARDCODE_REG_KEYS
import System.IO.Unsafe (unsafePerformIO)
import qualified Foreign.C as C
#endif

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- * The Auxiliary Library

-- | Key, in the registry, for table of loaded modules.
loadedTableRegistryField :: String
#ifdef HARDCODE_REG_KEYS
loadedTableRegistryField = "_LOADED"
#else
loadedTableRegistryField = unsafePerformIO (C.peekCString c_loaded_table)
{-# NOINLINE loadedTableRegistryField #-}

foreign import capi unsafe "lauxlib.h value LUA_LOADED_TABLE"
  c_loaded_table :: CString
#endif

-- | Key, in the registry, for table of preloaded loaders.
preloadTableRegistryField :: String
#ifdef HARDCODE_REG_KEYS
preloadTableRegistryField = "_PRELOAD"
#else
preloadTableRegistryField = unsafePerformIO (C.peekCString c_preload_table)
{-# NOINLINE preloadTableRegistryField #-}

foreign import capi unsafe "lauxlib.h value LUA_PRELOAD_TABLE"
  c_preload_table :: CString
#endif

foreign import capi SAFTY "lauxlib.h luaL_getmetafield"
  luaL_getmetafield :: Lua.State -> StackIndex -> CString -> IO Lua.TypeCode

foreign import capi SAFTY "lauxlib.h luaL_getmetatable"
  luaL_getmetatable :: Lua.State -> CString -> IO Lua.TypeCode

foreign import capi SAFTY "lauxlib.h luaL_loadbuffer"
  luaL_loadbuffer :: Lua.State -> Ptr CChar -> CSize -> CString
                  -> IO Lua.StatusCode

foreign import ccall SAFTY "lauxlib.h luaL_newmetatable"
  luaL_newmetatable :: Lua.State -> CString -> IO Lua.LuaBool

foreign import ccall unsafe "lauxlib.h hsluaL_newstate"
  hsluaL_newstate :: IO Lua.State

foreign import ccall SAFTY "lauxlib.h luaL_ref"
  luaL_ref :: Lua.State -> StackIndex -> IO CInt

foreign import ccall safe "hslua.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

foreign import capi SAFTY "lauxlib.h luaL_traceback"
  luaL_traceback :: Lua.State -> Lua.State -> CString -> CInt -> IO ()

foreign import ccall SAFTY "lauxlib.h luaL_unref"
  luaL_unref :: Lua.State -> StackIndex -> CInt -> IO ()

-- | See
-- <https://www.lua.org/manual/5.3/manual.html#luaL_testudata luaL_testudata>
foreign import capi SAFTY "lauxlib.h luaL_testudata"
  luaL_testudata :: Lua.State -> StackIndex -> CString -> IO (Ptr ())

--
-- References
--

-- | Reference to a stored value.
data Reference =
    Reference CInt -- ^ Reference to a stored value
  | RefNil         -- ^ Reference to a nil value
  deriving (Eq, Show)

foreign import capi SAFTY "lauxlib.h value LUA_REFNIL"
  refnil :: CInt

-- | Convert a reference to its C representation.
fromReference :: Reference -> CInt
fromReference = \case
  Reference x -> x
  RefNil      -> refnil

-- | Create a reference from its C representation.
toReference :: CInt -> Reference
toReference x =
  if x == refnil
  then RefNil
  else Reference x
