{-|
Module      : Foreign.Lua.Module
Copyright   : © 2019-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions for HsLua modules.
-}
module Foreign.Lua.Module
  ( requirehs
  , preloadhs
  , addfield
  , addfunction
  , create
  )
where

import Control.Monad (unless)
import Foreign.Lua.Core
import Foreign.Lua.Types (Pushable, push)
import Foreign.Lua.FunctionCalling (ToHaskellFunction, pushHaskellFunction)

-- | Load a module, defined by a Haskell action, under the given name.
--
-- Similar to @luaL_required@: After checking "loaded" table, calls
-- @pushMod@ to push a module to the stack, and registers the result in
-- @package.loaded@ table.
--
-- The @pushMod@ function must push exactly one element to the top of
-- the stack. This is not checked, but failure to do so will lead to
-- problems. Lua's @package@ module must have been loaded by the time
-- this function is invoked.
--
-- Leaves a copy of the module on the stack.
requirehs :: String -> Lua () -> Lua ()
requirehs modname pushMod = do
  -- get table of loaded modules
  getfield registryindex loadedTableRegistryField

  -- Check whether module has already been loaded.
  getfield stackTop modname  -- LOADED[modname]
  alreadyLoaded <- toboolean stackTop

  unless alreadyLoaded $ do
    pop 1  -- remove field
    pushMod  -- push module
    pushvalue stackTop  -- make copy of module
    -- add module under the given name (LOADED[modname] = module)
    setfield (nthFromTop 3) modname

  remove (nthFromTop 2)  -- remove table of loaded modules

-- | Registers a preloading function. Takes an module name and the Lua
-- operation which produces the package.
preloadhs :: String -> Lua NumResults -> Lua ()
preloadhs name pushMod = do
  getfield registryindex preloadTableRegistryField
  pushHaskellFunction pushMod
  setfield (nthFromTop 2) name
  pop 1

-- | Add a string-indexed field to the table at the top of the stack.
addfield :: Pushable a => String -> a -> Lua ()
addfield name value = do
  push name
  push value
  rawset (nthFromTop 3)

-- | Attach a function to the table at the top of the stack, using the
-- given name.
addfunction :: ToHaskellFunction a => String -> a -> Lua ()
addfunction name fn = do
  push name
  pushHaskellFunction fn
  rawset (nthFromTop 3)

-- | Create a new module (i.e., a Lua table).
create :: Lua ()
create = newtable
