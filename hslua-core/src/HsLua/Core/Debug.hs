{-|
Module      : HsLua.Core.Debug
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Bindings to Lua's debug interface.
-}
module HsLua.Core.Debug
  ( getupvalue
  , setupvalue
  ) where

import Control.Monad ((<$!>))
import Foreign.C (CString)
import Foreign.Ptr (nullPtr)
import HsLua.Core.Types (LuaE, Name (Name), StackIndex, liftLua)
import Lua.Debug (lua_getupvalue, lua_setupvalue)
import qualified Data.ByteString as B

-- | Gets information about the @n@-th upvalue of the closure at index
-- @funcindex@. It pushes the upvalue's value onto the stack and returns
-- its name. Returns 'Nothing' (and pushes nothing) when the index @n@
-- is greater than the number of upvalues.
--
-- See
-- <https://www.lua.org/manual/5.4/manual.html#pdf-debug.getupvalue debug.getupvalue>
-- for more information about upvalues.
--
-- @[0, +(0|1), -]@
--
-- Wraps 'lua_getupvalue'.
getupvalue :: StackIndex   -- ^ funcindex
           -> Int          -- ^ n
           -> LuaE e (Maybe Name)
getupvalue idx n = liftLua $ \l ->
  lua_getupvalue l idx (fromIntegral n) >>= toMaybeName

-- | Sets the value of a closure’s upvalue. It assigns the value on the
-- top of the stack to the upvalue and returns its name. It also pops
-- the value from the stack.
--
-- Returns 'Nothing' (and pops nothing) when the index @n@ is greater
-- than the number of upvalues.
--
-- Parameters @funcindex@ and @n@ are as in the function 'getupvalue'.
--
-- @[-(0|1), +0, -]@
--
-- Wraps 'lua_setupvalue'.
setupvalue :: StackIndex   -- ^ funcindex
           -> Int          -- ^ n
           -> LuaE e (Maybe Name)
setupvalue idx n = liftLua $ \l ->
  lua_setupvalue l idx (fromIntegral n) >>= toMaybeName

-- | Convert a (possibly @NULL@) null-terminated C string to a name.
toMaybeName :: CString -> IO (Maybe Name)
toMaybeName cstr =
  if cstr == nullPtr
    then return Nothing
    else Just . Name <$!> B.packCString cstr
