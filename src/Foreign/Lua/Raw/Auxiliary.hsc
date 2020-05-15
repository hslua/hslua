{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Raw.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to functions and constants of the auxiliary library.
-}
module Foreign.Lua.Raw.Auxiliary
  ( c_loaded_table
  , c_preload_table
  , hsluaL_tolstring
  , luaL_getmetafield
  , luaL_getmetatable
  , luaL_loadbuffer
  , luaL_newmetatable
  , luaL_newstate
  , luaL_ref
  , luaL_traceback
  , luaL_unref
    -- * Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
  ) where

import Foreign.C (CChar, CInt (CInt), CSize (CSize), CString)
import Foreign.Lua.Raw.Types (StackIndex)
import Foreign.Ptr
import qualified Foreign.Lua.Raw.Types as Lua

#ifndef HARDCODE_REG_KEYS
import System.IO.Unsafe (unsafePerformIO)
import qualified Foreign.C as C
#endif

##ifdef ALLOW_UNSAFE_GC
##define SAFTY unsafe
##else
##define SAFTY safe
##endif


--------------------------------------------------------------------------------
-- * The Auxiliary Library

-- | Key, in the registry, for table of loaded modules.
loadedTableRegistryField :: String
#ifdef HARDCODE_REG_KEYS
loadedTableRegistryField = "_LOADED"
#else
loadedTableRegistryField = unsafePerformIO (C.peekCString c_loaded_table)
{-# NOINLINE loadedTableRegistryField #-}

foreign import capi "lauxlib.h value LUA_LOADED_TABLE"
  c_loaded_table :: CString
#endif

-- | Key, in the registry, for table of preloaded loaders.
preloadTableRegistryField :: String
#ifdef HARDCODE_REG_KEYS
preloadTableRegistryField = "_PRELOAD"
#else
preloadTableRegistryField = unsafePerformIO (C.peekCString c_preload_table)
{-# NOINLINE preloadTableRegistryField #-}

foreign import capi "lauxlib.h value LUA_PRELOAD_TABLE"
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

foreign import ccall unsafe "lauxlib.h luaL_newstate"
  luaL_newstate :: IO Lua.State

foreign import ccall SAFTY "lauxlib.h luaL_ref"
  luaL_ref :: Lua.State -> StackIndex -> IO CInt

foreign import ccall safe "error-conversion.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

foreign import capi unsafe "lauxlib.h luaL_traceback"
  luaL_traceback :: Lua.State -> Lua.State -> CString -> CInt -> IO ()

foreign import ccall SAFTY "lauxlib.h luaL_unref"
  luaL_unref :: Lua.State -> StackIndex -> CInt -> IO ()
