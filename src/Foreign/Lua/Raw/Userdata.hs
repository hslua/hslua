{-|
Module      : Foreign.Lua.Raw.Userdata
Copyright   : © 2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Bindings to HsLua-specific functions used to push Haskell values
as userdata.
-}
module Foreign.Lua.Raw.Userdata
  ( hslua_newuserdata
  , hslua_userdata_gc_ptr
  ) where

import Foreign.C (CInt (CInt), CString)
import Foreign.StablePtr (StablePtr)
import Foreign.Lua.Raw.Types
  ( CFunction
  , LuaBool (LuaBool)
  , State (State)
  )

-- | Convert callable userdata at top of stack into a CFunction,
-- translating errors to Lua errors. Use with @'pushcclosure'@.
foreign import ccall safe "hslua.h hslua_newuserdata"
  hslua_newuserdata :: State       -- ^ Lua state
                    -> StablePtr a -- ^ Pointer to Haskell data
                    -> CString     -- ^ Userdata name (__name)
                    -> IO LuaBool  -- ^ True iff new metatable was created

-- | Function to free the stable pointer in a userdata, ensuring the Haskell
-- value can be garbage collected. This function does not call back into
-- Haskell, making is safe to call even from functions imported as unsafe.
foreign import ccall safe "&hslua_userdata_gc"
  hslua_userdata_gc_ptr :: CFunction
