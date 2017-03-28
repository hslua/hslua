{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Foreign.Lua as Lua

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.getglobal l "print"
    Lua.pushstring l "Hello from"
    Lua.getglobal l "_VERSION"
    Lua.call l 2 0
    Lua.getglobal l "jit"
    isLuajit <- Lua.istable l (-1)
    when isLuajit $ do
        Lua.getglobal l "print"
        Lua.getfield l (-2) "version"
        Lua.call l 1 0
    Lua.close l
