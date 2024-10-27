{-|
Module      : Main
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Test marshaling/unmarshaling from and to the Lua stack.
-}
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLua.ObjectOrientationTests
import qualified HsLua.ObjectOrientation.SumTypeTests

main :: IO ()
main = defaultMain $ testGroup "hslua-objectorientation" tests

-- | HSpec tests
tests :: [TestTree]
tests =
  [ HsLua.ObjectOrientationTests.tests
  , HsLua.ObjectOrientation.SumTypeTests.tests
  ]
