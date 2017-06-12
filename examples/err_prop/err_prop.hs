{-# LANGUAGE OverloadedStrings #-}

-- An example program that demonstrates error propagation between Haskell and
-- Lua. It creates a function call stack with 10 functions, like this:
--
--   Lua function
--   ...
--   Haskell function
--   Lua function
--   Program
--
-- And then the function at the top throws an error, according to the error
-- conventions described in the docs. Error is propagated to the program at the
-- bottom.
--
-- Then same thing happens, starting with Haskell function:
--
--   Haskell function
--   ...
--   Lua function
--   Haskell function
--   Program

import qualified Data.ByteString.Char8 as BC
import Control.Monad.Reader (liftIO)
import Foreign.C.Types (CInt)
import Foreign.Lua as Lua

main :: IO ()
main = runLua $ do
  openlibs
  registerrawhsfunction "fail_when_zero_haskell" failWhenZero

  -- Define the Lua function
  loadfile "examples/err_prop/err_prop.lua"
  call 0 0

  -- Start the loop by calling Lua function with argument 10
  getglobal "fail_when_zero"
  pushinteger 10
  -- Since Lua function will be the one that propagates error to the program,
  -- we need to catch it using `pcall`
  ret <- pcall 1 1 0
  errMsg <- tostring 1
  liftIO $ putStrLn $ "ret: " ++ show ret -- TODO: Implement pcall return values as a type
  liftIO $ putStrLn $ "errMsg: " ++ BC.unpack errMsg

  top <- gettop
  liftIO $ putStrLn $ "top: " ++ show top
  pop 1

  -- start the loop by calling Haskell function with argument 10
  getglobal "fail_when_zero_haskell"
  pushinteger 10
  -- Our convention is that Haskell functions never use `lua_error` because
  -- it's never safe(it's not even exported by the library for this reason).
  -- So if we're calling a Haskell function that `pcall` and `call` does the
  -- same thing.
  call 1 2
  -- We know it failed, so just read the error message without checking first
  -- argument
  errMsg <- tostring 2
  liftIO $ putStrLn $ "errMsg: " ++ BC.unpack errMsg
  pop 2

failWhenZero :: LuaState -> IO CInt
failWhenZero l = runLuaWith l $ do
  i <- tointeger 1
  liftIO $ putStrLn $ "Haskell: " ++ show i
  if i == 0
    then pushstring "Failing from Haskell" >> fmap fromIntegral lerror
    else do
      getglobal "fail_when_zero"
      pushinteger (i - 1)
      ret <- pcall 1 1 0
      if ret /= 0
        then
          -- propagate the error. no need to push error message since it's
          -- already at the top of the stack at this point. (because of how
          -- `pcall` works)
          fmap fromIntegral lerror
        else
          -- Lua function's return value is on the stack, return it
          return 1
