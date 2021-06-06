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
    openlibs             -- load the default Lua packages
    dostring luaProgram  -- load and run the program
  putStrLn ("Lua finished with status '" ++ show luaStatus ++ "'.")


-- | The Lua program. It uses a simple loop to calculate and print the
-- first 11 Fibonacci numbers.
--
-- Note that we are using the /OverloadedStrings/ extension. The
-- @IsString@ instance for ByteString behaves weirdly with non-ASCII
-- characters, but we don't use any here.
luaProgram :: B.ByteString
luaProgram = B.concat
  [ "local a, b = 0, 1\n"
  , "for i = 0, 10 do\n"
  , "  print(i, a)\n"
  , "  a, b = b, a + b\n"
  , "end\n"
  ]
