{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Call
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to function call helpers.
-}
module Foreign.Lua.Raw.Call
  ( PreCFunction
  , hslua_newhsfunwrapper
  ) where

import Foreign.C (CInt (CInt))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr)
import Foreign.Storable (peek)
import Foreign.Lua.Raw.Types
  ( NumResults (NumResults)
  , State (State)
  )

-- | Type of raw Haskell functions that can be made into
-- 'CFunction's.
type PreCFunction = State -> IO NumResults

-- | Retrieve the pointer to a Haskell function from the wrapping
-- userdata object.
foreign import ccall "hslua.h hslua_hs_fun_ptr"
  hslua_hs_fun_ptr :: State -> IO (Ptr ())

foreign import ccall "hslua.h hslua_newhsfunwrapper"
  hslua_newhsfunwrapper :: State -> StablePtr a -> IO ()

-- | Call the Haskell function stored in the userdata. This
-- function is exported as a C function, as the C code uses it as
-- the @__call@ value of the wrapping userdata metatable.
hslua_call_wrapped_hs_fun :: State -> IO NumResults
hslua_call_wrapped_hs_fun l = do
  udPtr <- hslua_hs_fun_ptr l
  if udPtr == nullPtr
    then error "Cannot call function; corrupted Lua object!"
    else do
      fn <- peek (castPtr udPtr) >>= deRefStablePtr
      fn l

foreign export ccall hslua_call_wrapped_hs_fun :: PreCFunction
