{-# LANGUAGE CPP #-}
{-|
Module      : Lua.Userdata
Copyright   : © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Bindings to HsLua-specific functions used to push Haskell values
as userdata.
-}
module Lua.Userdata
  ( hslua_fromuserdata
  , hslua_newhsuserdatauv
  , hslua_newudmetatable
  , hslua_putuserdata
  ) where

import Foreign.C (CInt (CInt), CString)
import Lua.Auxiliary (luaL_testudata)
import Lua.Primary (lua_newuserdatauv)
import Lua.Types
  ( LuaBool (..)
  , StackIndex (..)
  , State (..)
  )
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.StablePtr (newStablePtr, deRefStablePtr, freeStablePtr)
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

-- | Creates a new userdata wrapping the given Haskell object, with
-- @nuvalue@ associated Lua values (uservalues).
hslua_newhsuserdatauv :: State
                      -> a      -- ^ value to be wrapped
                      -> CInt   -- ^ nuvalue
                      -> IO ()
hslua_newhsuserdatauv l x nuvalue = do
  xPtr <- newStablePtr x
  udPtr <- lua_newuserdatauv l (fromIntegral $ sizeOf xPtr) nuvalue
  poke (castPtr udPtr) xPtr
{-# INLINABLE hslua_newhsuserdatauv #-}

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

-- | Replaces the Haskell value contained in the userdata value at
-- @index@. Checks that the userdata is of type @name@ and returns
-- 'True' on success, or 'False' otherwise.
hslua_putuserdata :: State
                  -> StackIndex  -- ^ index
                  -> CString     -- ^ name
                  -> a           -- ^ new Haskell value
                  -> IO Bool
hslua_putuserdata l idx name x = do
  xPtr <- newStablePtr x
  udPtr <- luaL_testudata l idx name
  if udPtr == nullPtr
    then return False
    else do
      peek (castPtr udPtr) >>= freeStablePtr
      poke (castPtr udPtr) xPtr
      return True
{-# INLINABLE hslua_putuserdata #-}
