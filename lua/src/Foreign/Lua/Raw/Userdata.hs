{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Userdata
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Bindings to HsLua-specific functions used to push Haskell values
as userdata.
-}
module Foreign.Lua.Raw.Userdata
  ( hslua_fromuserdata
  , hslua_newhsuserdata
  , hslua_newudmetatable
  ) where

import Foreign.C (CInt (CInt), CString)
import Foreign.Lua.Raw.Auxiliary (luaL_testudata)
import Foreign.Lua.Raw.Functions (lua_newuserdata)
import Foreign.Lua.Raw.Types
  ( LuaBool (..)
  , StackIndex (..)
  , State (..)
  )
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Storable (peek, poke, sizeOf)

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- | Creates and registers a new metatable for a userdata-wrapped
-- Haskell value; checks whether a metatable of that name has been
-- registered yet and uses the registered table if possible.
foreign import ccall SAFTY "hsludata.h hslua_newudmetatable"
  hslua_newudmetatable :: State       -- ^ Lua state
                       -> CString     -- ^ Userdata name (__name)
                       -> IO LuaBool  -- ^ True iff new metatable
                                      --   was created.

-- | Creates a new userdata wrapping the given Haskell object.
hslua_newhsuserdata :: State -> a -> IO ()
hslua_newhsuserdata l x = do
  xPtr <- newStablePtr x
  udPtr <- lua_newuserdata l (fromIntegral $ sizeOf xPtr)
  poke (castPtr udPtr) xPtr
{-# INLINABLE hslua_newhsuserdata #-}

-- | Retrieves a Haskell object from userdata at the given index.
-- The userdata /must/ have the given name.
hslua_fromuserdata :: State
                   -> StackIndex  -- ^ userdata index
                   -> CString     -- ^ name
                   -> IO (Maybe a)
hslua_fromuserdata l idx name = do
  udPtr <- luaL_testudata l idx name
  if udPtr == nullPtr
    then return Nothing
    else Just <$> (peek (castPtr udPtr) >>= deRefStablePtr)
{-# INLINABLE hslua_fromuserdata #-}
