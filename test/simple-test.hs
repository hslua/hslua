{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Foreign.Lua as Lua

main :: IO ()
main = Lua.runLua $ do
  Lua.openlibs
  Lua.getglobal "print"
  Lua.pushstring "Hello from"
  Lua.getglobal "_VERSION"
  Lua.call 2 0
  Lua.getglobal "jit"
  isLuajit <- Lua.istable (-1)
  when isLuajit $ do
    Lua.getglobal "print"
    Lua.getfield (-2) "version"
    Lua.call 1 0
