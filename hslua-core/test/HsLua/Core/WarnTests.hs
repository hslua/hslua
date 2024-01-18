{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.WarnTests
Copyright   :  © 2017-2024 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb@hslua.org>

Check that setting hook for warning messages works.
-}
module HsLua.Core.WarnTests (tests) where

import HsLua.Core
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Warn"
  [ "warnings get handled" =:
    Just "Hi Mom!" `shouldBeResultOf` do
      openlibs
      setwarnf' $ \msg -> do
        pushstring msg
        setfield registryindex "hslua testing"
      stat <- dostring "warn('Hi', ' ', 'Mom!')"
      case stat of
        OK -> do
          getfield registryindex "hslua testing"
          tostring top
        _  -> do
          throwErrorAsException
  ]
