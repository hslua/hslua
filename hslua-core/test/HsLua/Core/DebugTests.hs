{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  HsLua.Core.DebugTests
Copyright   :  © 2023-2024 Albert Krewinkel
License     :  MIT
Maintainer  :  Albert Krewinkel <tarleb@hslua.org>

Test the debug interface.
-}
module HsLua.Core.DebugTests (tests) where

import HsLua.Core as Lua
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Debug"
  [ "getupvalue" =:
    Just "x" `shouldBeResultOf` do
      loadstring "local x = 0; return function () return x + 23 end"
      call 0 1
      getupvalue top 1

  , "setupvalue" =:
    (Just "i", Just 28) `shouldBeResultOf` do
      loadstring "local i = 0; return function () return i + 23 end"
      call 0 1
      -- set 'x' to 5
      pushinteger 5
      name <- setupvalue (nth 2) 1
      -- call function and check the returned value
      call 0 1
      i <- tointeger top
      return (name, i)
  ]
