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
import qualified HsLua.FunctionCallingTests
import qualified HsLua.ModuleTests
import qualified HsLua.PeekTests
import qualified HsLua.PushTests
import qualified HsLua.TypesTests
import qualified HsLua.Types.PeekableTests
import qualified HsLua.Types.PushableTests
import qualified HsLua.UserdataTests
import qualified HsLua.UtilTests

main :: IO ()
main = defaultMain $ testGroup "hslua" tests

-- | HSpec tests
tests :: [TestTree]
tests =
  [ HsLua.PushTests.tests
  , HsLua.FunctionCallingTests.tests
  , HsLua.UtilTests.tests
  , testGroup "Sending and receiving values from the stack"
    [ HsLua.TypesTests.tests
    , HsLua.Types.PeekableTests.tests
    , HsLua.Types.PushableTests.tests
    ]
  , HsLua.UserdataTests.tests
  , HsLua.ModuleTests.tests
  , HsLua.CallTests.tests
  , HsLuaTests.tests
  , HsLua.PeekTests.tests
  ]
