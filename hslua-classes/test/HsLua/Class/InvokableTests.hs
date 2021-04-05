{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Class.InvokableTests
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that Lua functions can be called from Haskell.
-}
module HsLua.Class.InvokableTests (tests) where

import Data.ByteString.Char8 as Char8
import HsLua.Class.Invokable (invoke)
import HsLua.Core (Lua, openlibs)
import Test.Tasty.HsLua ((=:), shouldBeErrorMessageOf, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Invokable"
  [ "test equality within lua" =:
    True `shouldBeResultOf` do
      openlibs
      invoke @Lua.Exception "rawequal" (5 :: Lua.Integer) (5.0 :: Lua.Number)

  , "failing lua function call" =:
    "foo" `shouldBeErrorMessageOf` do
      openlibs
      invoke @Lua.Exception "assert" False (Char8.pack "foo") :: Lua Bool

  , "pack table via lua procedure" =:
    (True, 23 :: Lua.Integer, "moin" :: ByteString) `shouldBeResultOf` do
      openlibs
      invoke @Lua.Exception "table.pack" True (23 :: Lua.Integer) (Char8.pack "moin")

  , "failing lua procedure call" =:
    "foo" `shouldBeErrorMessageOf` do
      openlibs
      invoke @Lua.Exception "error" (Char8.pack "foo") :: Lua ()

  , "Error when Lua-to-Haskell result conversion fails" =:
    "string expected, got boolean" `shouldBeErrorMessageOf` do
      openlibs
      invoke @Lua.Exception "rawequal" (Char8.pack "a") () :: Lua String
  ]
