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
  ( HsFunction
  , hslua_newhsfunction
  , hslua_pushhsfunction
  ) where

import Foreign.C (CInt (CInt))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import Foreign.Storable (peek)
import Foreign.Lua.Raw.Types
  ( NumResults (NumResults)
  , State (State)
  )

##ifdef ALLOW_UNSAFE_GC
##define SAFTY unsafe
##else
##define SAFTY safe
##endif

-- | Type of raw Haskell functions that can be made into
-- 'CFunction's.
type HsFunction = State -> IO NumResults

-- | Retrieve the pointer to a Haskell function from the wrapping
-- userdata object.
foreign import ccall SAFTY "hslua.h hslua_hs_fun_ptr"
  hslua_hs_fun_ptr :: State -> IO (Ptr ())

-- | Pushes a new C function created from an 'HsFunction'.
foreign import ccall SAFTY "hslua.h hslua_newhsfunction"
  hslua_newhsfunction :: State -> StablePtr a -> IO ()

hslua_pushhsfunction :: State -> HsFunction -> IO ()
hslua_pushhsfunction l preCFn =
  newStablePtr preCFn >>= hslua_newhsfunction l
{-# INLINABLE hslua_pushhsfunction #-}

-- | Call the Haskell function stored in the userdata. This
-- function is exported as a C function, as the C code uses it as
-- the @__call@ value of the wrapping userdata metatable.
hslua_call_wrapped_hs_fun :: HsFunction
hslua_call_wrapped_hs_fun l = do
  udPtr <- hslua_hs_fun_ptr l
  if udPtr == nullPtr
    then error "Cannot call function; corrupted Lua object!"
    else do
      fn <- peek (castPtr udPtr) >>= deRefStablePtr
      fn l

foreign export ccall hslua_call_wrapped_hs_fun :: HsFunction
