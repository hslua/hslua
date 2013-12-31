-- | added for issue #13
module Main where

import qualified Scripting.Lua               as Lua

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.pushstring l "A\NULB"
    putStrLn =<< Lua.tostring l 1
    Lua.close l
