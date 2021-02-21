{-|
Module      : Foreign.Lua
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

Extend Haskell programs with a Lua interpreter.
-}
module Foreign.Lua
  ( withNewState
    -- * Stack index helpers
  , nthTop
  , nthBottom
  , nth
  , top
    -- * Standard Lua libraries
  , luaopen_base
  , luaopen_table
  , luaopen_io
  , luaopen_os
  , luaopen_string
  , luaopen_math
  , luaopen_debug
  , luaopen_package
  ) where

import Foreign.C (CInt)
import Foreign.Lua.Auxiliary (hsluaL_newstate)
import Foreign.Lua.Functions (lua_close)
import Foreign.Lua.Lib
import Foreign.Lua.Types (StackIndex (StackIndex), State)

-- | Runs operations on a new Lua @'Lua.State'@. The state is created
-- when the function is called and closed on return. The state, and all
-- pointers to values within it, *must not* be used after the function
-- returns.
withNewState :: (State -> IO a) -> IO a
withNewState f = do
  l <- hsluaL_newstate
  result <- f l
  lua_close l
  return result

--
-- Stack index helpers
--

-- | Stack index of the nth element from the top of the stack.
nthTop :: CInt -> StackIndex
nthTop n = StackIndex (-n)
{-# INLINABLE nthTop #-}

-- | Stack index of the nth element from the bottom of the stack.
nthBottom :: CInt -> StackIndex
nthBottom = StackIndex
{-# INLINABLE nthBottom #-}

-- | Alias for 'nthTop'.
nth :: CInt -> StackIndex
nth = nthTop
{-# INLINABLE nth #-}

-- | Index of the topmost stack element.
top :: StackIndex
top = nthTop 1
{-# INLINABLE top #-}
