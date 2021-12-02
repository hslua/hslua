{-# OPTIONS_GHC -Wno-unused-do-bind        #-}
{-|
Module      : Lua.PrimaryTests
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for bindings to primary API functions.
-}
module Lua.PrimaryTests (tests) where

import Lua
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@=?) )

-- | Tests for unsafe methods.
tests :: TestTree
tests = testGroup "Primary"
  [ testGroup "C functions"
    [ "can push and call luaopen_math" =: do
        LUA_TTABLE `shouldBeResultOf` \l -> do
          lua_pushcfunction l luaopen_math
          lua_pcall l 0 1 0
          lua_type l (-1)
    ]
  ]

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase

shouldBeResultOf :: (HasCallStack, Eq a, Show a)
                 => a -> (State -> IO a) -> Assertion
shouldBeResultOf expected luaOp = do
  result <- withNewState luaOp
  expected @=? result
