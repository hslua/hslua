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
import Foreign.C.Types (CInt)
import Scripting.Lua as Lua

main :: IO ()
main = do
    l <- newstate
    openlibs l
    registerrawhsfunction l "fail_when_zero_haskell" failWhenZero

    -- Define the Lua function
    loadfile l "examples/err_prop/err_prop.lua"
    call l 0 0

    -- Start the loop by calling Lua function with argument 10
    getglobal l "fail_when_zero"
    pushinteger l 10
    -- Since Lua function will be the one that propagates error to the program,
    -- we need to catch it using `pcall`
    ret <- pcall l 1 1 0
    errMsg <- tostring l 1
    putStrLn $ "ret: " ++ show ret -- TODO: Implement pcall return values as a type
    putStrLn $ "errMsg: " ++ BC.unpack errMsg

    top <- gettop l
    putStrLn $ "top: " ++ show top
    pop l 1

    -- start the loop by calling Haskell function with argument 10
    getglobal l "fail_when_zero_haskell"
    pushinteger l 10
    -- Our convention is that Haskell functions never use `lua_error` because
    -- it's never safe(it's not even exported by the library for this reason).
    -- So if we're calling a Haskell function that `pcall` and `call` does the
    -- same thing.
    call l 1 2
    -- We know it failed, so just read the error message without checking first
    -- argument
    errMsg <- tostring l 2
    putStrLn $ "errMsg: " ++ BC.unpack errMsg
    pop l 2

    close l

failWhenZero :: LuaState -> IO CInt
failWhenZero l = do
    i <- tointeger l 1
    putStrLn $ "Haskell: " ++ show i
    if i == 0
      then pushstring l "Failing from Haskell" >> fmap fromIntegral (lerror l)
      else do
        getglobal l "fail_when_zero"
        pushinteger l (i - 1)
        ret <- pcall l 1 1 0
        if ret /= 0
          then
            -- propagate the error. no need to push error message since it's
            -- already at the top of the stack at this point. (because of how
            -- `pcall` works)
            fmap fromIntegral (lerror l)
          else
            -- Lua function's return value is on the stack, return it
            return 1
