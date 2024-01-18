{-|
Module      : Lua.Debug
Copyright   : © 2023-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Haskell bindings to Lua's debug interface.
-}
module Lua.Debug
  ( lua_getupvalue
  , lua_setupvalue
  )
where

import Foreign.C
import Lua.Types as Lua

-- | Gets information about the @n@-th upvalue of the closure at index
-- @funcindex@. It pushes the upvalue\'s value onto the stack and
-- returns its name. Returns @NULL@ (and pushes nothing) when the index
-- @n@ is greater than the number of upvalues.
--
-- See
-- <https://www.lua.org/manual/5.4/manual.html#pdf-debug.getupvalue debug.getupvalue>
-- for more information about upvalues.
--
-- @[0, +(0|1), -]@
--
-- <https://www.lua.org/manual/5.4/manual.html#lua_getupvalue lua_getupvalue>.
foreign import ccall unsafe "lua.h lua_getupvalue"
  lua_getupvalue :: Lua.State
                 -> StackIndex  -- ^ funcindex
                 -> CInt        -- ^ n
                 -> IO CString

-- | Sets the value of a closure’s upvalue. It assigns the value on the
-- top of the stack to the upvalue and returns its name. It also pops
-- the value from the stack.
--
-- Returns @NULL@ (and pops nothing) when the index @n@ is greater than
-- the number of upvalues.
--
-- Parameters @funcindex@ and @n@ are as in the function
-- 'lua_getupvalue'.
--
-- @[-(0|1), +0, -]@
--
-- <https://www.lua.org/manual/5.4/manual.html#lua_setupvalue lua_setupvalue>.
foreign import ccall unsafe "lua.h lua_setupvalue"
  lua_setupvalue :: Lua.State
                 -> StackIndex  -- ^ funcindex
                 -> CInt        -- ^ n
                 -> IO CString
