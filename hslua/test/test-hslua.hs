{-|
Module      : Main
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for HsLua.
-}
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLuaTests
import qualified HsLua.CallTests
import qualified HsLua.ModuleTests
import qualified HsLua.UtilTests

main :: IO ()
main = defaultMain $ testGroup "hslua" tests

-- | HSpec tests
tests :: [TestTree]
tests =
  [ HsLua.ModuleTests.tests
  , HsLua.CallTests.tests
  , HsLua.UtilTests.tests
  , HsLuaTests.tests
  ]
