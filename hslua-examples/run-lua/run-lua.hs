{-# LANGUAGE OverloadedStrings #-}
{-| Simple program running Lua code.
-}
module Main where
import HsLua.Core
import qualified Data.ByteString as B

main :: IO ()
main = do
  putStrLn "Calling Lua to output some Fibonacci numbers."
  luaStatus <- run $ do
    openlibs        -- load the default Lua packages
    dostring luaProgram
  putStrLn ("Lua finished with status '" ++ show luaStatus ++ "'.")


-- | The Lua program. It uses a coroutine to generate Fibonacci numbers,
-- printing the first 11 of them.
--
-- Note that we are using the /OverloadedStrings/ extension. The
-- @IsString@ instance for ByteString behaves weirdly with non-ASCII
-- characters, but we don't use any here.
luaProgram :: B.ByteString
luaProgram = B.concat
  [ "local fibs = coroutine.create(function ()\n"
  , "    local a, b = 0, 1\n"
  , "    while true do\n"
  , "      coroutine.yield(a)\n"
  , "      a, b = b, a + b\n"
  , "    end\n"
  , "end)\n"
  , "\n"
  , "for i = 0, 10 do\n"
  , "  print(i, select(2, coroutine.resume(fibs)))\n"
  , "end\n"
  ]
