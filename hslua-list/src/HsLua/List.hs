{-# LANGUAGE LambdaCase           #-}
{- |
Copyright  : © 2021-2023 Albert Krewinkel
License    : MIT
Maintainer : Albert Krewinkel <tarleb@hslua.org>

Lua lists with additional methods.
-}
module HsLua.List
  ( luaopen_list_ptr
  , pushListModule
  , newListMetatable
  ) where

import Data.ByteString (useAsCString)
import Foreign.C
import HsLua.Core

-- | Pointer to the function that opens the List module and pushes it to the
-- stack.
foreign import ccall unsafe "listmod.c &luaopen_list"
  luaopen_list_ptr :: CFunction

-- | Opens the List module and pushes it to the stack.
pushListModule :: LuaError e => LuaE e ()
pushListModule = do
  pushcfunction luaopen_list_ptr
  call 0 1

-- | Creates a new list metatable with the given name.
foreign import ccall "listmod.c lualist_newmetatable"
  lualist_newmetatable :: State -> CString -> IO CInt

-- | Pushes the metatable of the given List type, creating it if
-- necessary. The @setup@ operation is run when the metatable did not
-- exists, was created, and is then at the top of the stack. The
-- operation may modify the table but must be balanced, and must leave
-- the stack as it found it.
newListMetatable :: Name -> LuaE e () {-^ setup -} -> LuaE e ()
newListMetatable (Name name) setup = do
  l <- state
  liftIO (useAsCString name (lualist_newmetatable l)) >>= \case
    0 -> pure ()   -- metatable already registered; no need to setup again
    _ -> setup
