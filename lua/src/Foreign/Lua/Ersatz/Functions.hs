{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Ersatz.Functions
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, CPP

Ersatz functions for Lua API items which may, directly or indirectly,
throw a Lua error.
-}
module Foreign.Lua.Ersatz.Functions
  ( hslua_compare
    -- * Get functions (Lua -> stack)
  , hslua_gettable
  , hslua_getglobal
    -- * Set functions (stack -> Lua)
  , hslua_settable
  , hslua_setglobal
    -- * Misc
  , hslua_error
  , hslua_next
  , hslua_concat
  )where

import Foreign.C
import Foreign.Lua.Types as Lua
import Foreign.Ptr

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_compare \
-- @lua_compare@> which catches any Lua errors.
foreign import capi safe "hslua.h hslua_compare"
  hslua_compare :: Lua.State
                -> StackIndex -> StackIndex -> CInt
                -> Ptr StatusCode -> IO LuaBool

--
-- Get functions (Lua -> stack)
--

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>.
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_gettable"
  hslua_gettable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO ()


-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_getglobal"
  hslua_getglobal :: Lua.State -> CString -> CSize -> Ptr StatusCode -> IO ()

--
-- Set functions (stack -> Lua)
--

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_settable \
-- @lua_settable@> which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_settable"
  hslua_settable :: Lua.State -> StackIndex -> Ptr StatusCode -> IO ()

-- | Wrapper around <https://lua.org/manual/5.3/manual.html#lua_setglobal \
-- @lua_setglobal@> which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_setglobal"
  hslua_setglobal :: Lua.State -> CString -> CSize -> Ptr StatusCode -> IO ()

--
-- Miscellaneous functions
--

-- | Replacement for
-- <https://lua.org/manual/5.3/manual.html#lua_error lua_error>; it uses
-- the HsLua error signaling convention instead of raw Lua errors.
foreign import ccall SAFTY "hslua.h hslua_error"
  hslua_error :: Lua.State -> IO NumResults

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_next lua_next> which
-- catches any Lua errors.
foreign import ccall safe "hslua.h hslua_next"
  hslua_next :: Lua.State -> StackIndex -> Ptr StatusCode -> IO LuaBool

-- | Wrapper around
-- <https://lua.org/manual/5.3/manual.html#lua_concat lua_concat>
-- which catches any Lua errors.
foreign import ccall safe "hslua.h hslua_concat"
  hslua_concat :: Lua.State -> NumArgs -> Ptr StatusCode -> IO ()
