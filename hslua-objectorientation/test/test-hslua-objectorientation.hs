{-|
Module      : Main
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Test marshaling/unmarshaling from and to the Lua stack.
-}
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLua.ObjectOrientationTests

main :: IO ()
main = defaultMain $ testGroup "hslua-objectorientation" tests

-- | HSpec tests
tests :: [TestTree]
tests = [ HsLua.ObjectOrientationTests.tests ]
