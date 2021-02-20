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
  ) where

import Foreign.Lua.Auxiliary (hsluaL_newstate)
import Foreign.Lua.Functions (lua_close)
import Foreign.Lua.Types (State)

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
