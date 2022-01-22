{-|
Module      : HsLua.Core.Package
Copyright   : © 2019-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions for HsLua modules.
-}
module HsLua.Core.Package
  ( requirehs
  , preloadhs
  )
where

import Control.Monad (void)
import HsLua.Core.Auxiliary
import HsLua.Core.Closures (pushHaskellFunction)
import HsLua.Core.Error (LuaError)
import HsLua.Core.Primary
import HsLua.Core.Types
-- import HsLua.Core.Utf8 (fromString)

-- | Load a module, defined by a Haskell action, under the given
-- name.
--
-- Similar to @luaL_requiref@: If @modname@ is not already present in
-- @package.loaded@, calls function @openf@ with string @modname@ as an
-- argument and sets the call result in @package.loaded[modname]@, as if
-- that function has been called through
-- <https://www.lua.org/manual/5.3/manual.html#pdf-require require>.
--
-- Leaves a copy of the module on the stack.
requirehs :: LuaError e
          => Name                 -- ^ modname
          -> (Name -> LuaE e ())  -- ^ openf
          -> LuaE e ()
requirehs modname openf = do
  void $ getsubtable registryindex loaded
  void $ getfield top modname
  toboolean top >>= \case
    True -> pure ()       -- package already loaded
    False -> do
      -- package not loaded, load it now
      pop 1  -- remove field
      oldtop <- gettop
      openf modname
      settop (oldtop + 1)
      pushvalue top  -- make copy of module (call result)
      setfield (nth 3) modname

  remove (nth 2)  -- remove LOADED table

-- | Registers a preloading function. Takes an module name and the
-- Lua operation which produces the package.
preloadhs :: LuaError e => Name -> LuaE e NumResults -> LuaE e ()
preloadhs name pushMod = do
  void $ getfield registryindex preload
  pushHaskellFunction pushMod
  setfield (nth 2) name
  pop 1
