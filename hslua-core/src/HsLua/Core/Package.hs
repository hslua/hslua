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
-- Similar to @luaL_required@: After checking "loaded" table,
-- calls @pushMod@ to push a module to the stack, and registers
-- the result in @package.loaded@ table.
--
-- The @pushMod@ function must push exactly one element to the top
-- of the stack. This is not checked, but failure to do so will
-- lead to problems. Lua's @package@ module must have been loaded
-- by the time this function is invoked.
--
-- Leaves a copy of the module on the stack.
requirehs :: LuaError e => Name -> LuaE e () -> LuaE e ()
requirehs modname pushMod = do
  -- get table of loaded modules
  void $ getfield registryindex loaded

  -- Check whether module has already been loaded.
  getfield top modname >>= \case -- LOADED[modname]
    TypeNil -> do    -- not loaded yet, load now
      pop 1          -- remove LOADED[modname], i.e., nil
      pushMod        -- push module
      pushvalue top  -- make copy of module
      -- add module under the given name (LOADED[modname] = module)
      setfield (nth 3) modname
    _ -> return ()

  remove (nth 2)  -- remove table of loaded modules

-- | Registers a preloading function. Takes an module name and the
-- Lua operation which produces the package.
preloadhs :: LuaError e => Name -> LuaE e NumResults -> LuaE e ()
preloadhs name pushMod = do
  void $ getfield registryindex preload
  pushHaskellFunction pushMod
  setfield (nth 2) name
  pop 1
