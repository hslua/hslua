{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : Main
Copyright   : © 2021-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

Tests for the list type.
-}
module Main (main) where

import HsLua.Core as Lua
import HsLua.List
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  listTests <- run @Lua.Exception $ do
    openlibs
    -- Init default List module
    pushListModule *> setglobal "List"
    -- Create a custom List type with constructor "CustomList"
    pushHaskellFunction $ do
      settop 1
      newListMetatable "CustomList" (pure ())
      setmetatable (nthBottom 1)
      return 1
    setglobal "CustomList"
    translateResultsFromFile "test/test-list.lua"

  defaultMain $ testGroup "hslua-list" [listTests]
