{-# OPTIONS_GHC -Wno-unused-do-bind        #-}
{-|
Module      : Lua.PrimaryTests
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for bindings to primary API functions.
-}
module Lua.PrimaryTests (tests) where

import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
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
  , testGroup "lua_stringtonumber"
    [ "converts a string to a number" =: do
        55 `shouldBeResultOf` \l -> do
          _ <- withCString "55" $ lua_stringtonumber l
          lua_tointegerx l top nullPtr
    , "returns length (incl NULL) of the string on success" =: do
        4 `shouldBeResultOf` \l -> do
          withCString "512" $ lua_stringtonumber l
    , "returns zero on failure" =: do
        0 `shouldBeResultOf` \l -> do
          withCString "NaN" $ lua_stringtonumber l
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
