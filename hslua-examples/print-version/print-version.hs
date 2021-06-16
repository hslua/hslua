-- @lua_getglobal@ is unsafe in general, but safe in this case.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-| Use low-level API functions to output the Lua version. -}
module Main where
import Control.Monad (void)
import Foreign.C (withCString)
import Lua

main :: IO ()
main = void . withNewState $ \l -> do
  luaL_openlibs l
  -- Prepare call `print(_VERSION)`: first push the `print` function
  -- to the stack, then the arguments.
  LUA_TFUNCTION <- withCString "print" (lua_getglobal l)
  LUA_TSTRING <- withCString "_VERSION" (lua_getglobal l)
  -- Call the function with one argument.
  lua_pcall l (NumArgs 1) (NumResults 0) (StackIndex 0)
