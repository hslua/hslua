{-|
Module      : Main
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Tests for the raw Lua bindings.
-}
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Runs tests.
main :: IO ()
main = defaultMain tests

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "hslua-core"
  []
