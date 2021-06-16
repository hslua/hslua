-- lua_setglobal is unsafe in general and causes a compiler warning.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-| Expose factorial function to Lua, using the low-level API. -}
module Main where
import Prelude
import Control.Monad (void)
import Foreign.C (withCString, withCStringLen)
import Foreign.Ptr (nullPtr)
import Lua

luaScript :: String
luaScript = unlines
  [ "print(' 5! =', factorial(5), type(factorial(5)))"
  , "print('30! =', factorial(30), type(factorial(30)))"
  ]

factorial :: PreCFunction
factorial l = do
  -- Get the first function argument as an integer.
  n <- lua_tointegerx l (nthBottom 1) nullPtr
  let result = product [1..fromIntegral n] :: Prelude.Integer
  -- push as integer if the result is small enough,
  -- and as string otherwise.
  if result <= fromIntegral (maxBound :: Lua.Integer)
    then lua_pushinteger l (fromIntegral result)
    else withCString (show result) (void . lua_pushstring l)
  -- Signal that one result value is on the top of the stack.
  return (NumResults 1)

main :: IO ()
main = void . withNewState $ \l -> do
  luaL_openlibs l
  -- expose Haskell function `factorial` as a Lua function of the same name.
  hslua_pushhsfunction l factorial
  _ <- withCString "factorial" (lua_setglobal l)
  -- Load the script.
  LUA_OK <- withCStringLen luaScript $ \(sPtr, sLen) ->
    withCString "script" $ luaL_loadbuffer l sPtr (fromIntegral sLen)
  -- Call the loaded script.
  LUA_OK <- lua_pcall l (NumArgs 0) LUA_MULTRET (StackIndex 0)
  return ()
