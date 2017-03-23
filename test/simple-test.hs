{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Scripting.Lua as Lua

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.getglobal l "print"
    Lua.pushstring l "Hello from"
    Lua.getglobal l "_VERSION"
    Lua.call l 2 0
    Lua.close l
