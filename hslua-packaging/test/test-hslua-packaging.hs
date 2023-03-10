{-|
Module      : Main
Copyright   : © 2020-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for hslua-packaging.
-}
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLua.PackagingTests

main :: IO ()
main = defaultMain tests

-- | Lua module packaging tests.
tests :: TestTree
tests = testGroup "Packaging" [HsLua.PackagingTests.tests]
