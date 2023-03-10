{-# LANGUAGE CPP #-}
{-|
Module      : Lua.Call
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Function to push Haskell functions as Lua C functions.

Haskell functions are converted into C functions in a two-step process.
First, a function pointer to the Haskell function is stored in a Lua
userdata object. The userdata gets a metatable which allows to invoke
the object as a function. The userdata also ensures that the function
pointer is freed when the object is garbage collected in Lua.

In a second step, the userdata is then wrapped into a C closure. The
wrapping function calls the userdata object and implements the error
protocol, converting special error values into proper Lua errors.
-}
module Lua.Call
  ( hslua_pushhsfunction
  ) where

import Foreign.C (CInt (CInt))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import Foreign.Storable (peek)
import Lua.Types
  ( NumResults (NumResults)
  , PreCFunction
  , State (State)
  )

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- | Retrieve the pointer to a Haskell function from the wrapping
-- userdata object.
foreign import ccall SAFTY "hslcall.c hslua_extracthsfun"
  hslua_extracthsfun :: State -> IO (Ptr ())

-- | Creates a new C function created from a 'PreCFunction'. The
-- function pointer to the PreCFunction is stored in a userdata object,
-- which is then wrapped by a C closure. The userdata object ensures
-- that the function pointer is freed when the function is garbage
-- collected in Lua.
foreign import ccall SAFTY "hslcall.c hslua_newhsfunction"
  hslua_newhsfunction :: State -> StablePtr a -> IO ()

-- | Pushes a Haskell operation as a Lua function. The Haskell operation
-- is expected to follow the custom error protocol, i.e., it must signal
-- errors with @'Lua.hslua_error'@.
--
-- === Example
-- Export the function to calculate triangular numbers.
--
-- > let triangular :: PreCFunction
-- >     triangular l' = do
-- >       n <- lua_tointegerx l' (nthBottom 1) nullPtr
-- >       lua_pushinteger l' (sum [1..n])
-- >       return (NumResults 1)
-- >
-- > hslua_newhsfunction l triangular
-- > withCString "triangular" (lua_setglobal l)
--
hslua_pushhsfunction :: State -> PreCFunction -> IO ()
hslua_pushhsfunction l preCFn =
  newStablePtr preCFn >>= hslua_newhsfunction l
{-# INLINABLE hslua_pushhsfunction #-}

-- | Call the Haskell function stored in the userdata. This
-- function is exported as a C function, as the C code uses it as
-- the @__call@ value of the wrapping userdata metatable.
hslua_callhsfun :: PreCFunction
hslua_callhsfun l = do
  udPtr <- hslua_extracthsfun l
  if udPtr == nullPtr
    then error "Cannot call function; corrupted Lua object!"
    else do
      fn <- peek (castPtr udPtr) >>= deRefStablePtr
      fn l

foreign export ccall hslua_callhsfun :: PreCFunction
