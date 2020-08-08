{-# LANGUAGE CPP #-}
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
  ( hslua_newudmetatable
  ) where

import Foreign.C (CInt (CInt), CString)
import Foreign.Lua.Raw.Types
  ( LuaBool (LuaBool)
  , State (State)
  )

##ifdef ALLOW_UNSAFE_GC
##define SAFTY unsafe
##else
##define SAFTY safe
##endif

-- | Convert callable userdata at top of stack into a CFunction,
-- translating errors to Lua errors. Use with @'pushcclosure'@.
foreign import ccall SAFTY "hsuserdata.h hslua_newudmetatable"
  hslua_newudmetatable :: State       -- ^ Lua state
                       -> CString     -- ^ Userdata name (__name)
                       -> IO LuaBool  -- ^ True iff new metatable
                                      --   was created.
