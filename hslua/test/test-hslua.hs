{-|
Module      : Main
Copyright   : © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for HsLua.
-}
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLuaTests
import qualified HsLua.UtilTests

main :: IO ()
main = defaultMain tests

-- | HSpec tests
tests :: TestTree
tests = testGroup "hslua"
  [ HsLua.UtilTests.tests
  , HsLuaTests.tests
  ]
