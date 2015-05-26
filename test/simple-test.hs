{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Scripting.Lua as Lua

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.getfield l Lua.globalsindex "print"
    Lua.pushstring l "Hello from"
    Lua.getfield l Lua.globalsindex "_VERSION"
    Lua.call l 2 0
    Lua.close l
