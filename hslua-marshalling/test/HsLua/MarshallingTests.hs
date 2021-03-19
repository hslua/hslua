{-|
Module      : HsLua.MarshallingTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Test marshalling of basic values.
-}
module HsLua.MarshallingTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified HsLua.Marshalling.PeekTests
import qualified HsLua.Marshalling.PushTests
import qualified HsLua.Marshalling.UserdataTests

-- | Tests for value marshalling.
tests :: TestTree
tests = testGroup "Marshalling"
  [ HsLua.Marshalling.PeekTests.tests
  , HsLua.Marshalling.PushTests.tests
  , HsLua.Marshalling.UserdataTests.tests
  ]
