{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.UtilTests
Copyright   :  © 2017-2023 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for utility types and functions
-}
module HsLua.UtilTests (tests) where

import HsLua
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Util"
  [ testGroup "getglobbal'"
    [ "returns nil if global does not exist" =:
      TypeNil `shouldBeResultOf` do
        getglobal' "nope"
        ltype top

    , "returns global" =:
      TypeTable `shouldBeResultOf` do
        openlibs
        getglobal' "math"
        ltype top

    , "can access nested fields" =:
      TypeFunction `shouldBeResultOf` do
        openlibs
        getglobal' "math.log"
        ltype top

    , "can access 3rd level fields" =:
      TypeTable `shouldBeResultOf` do
        openlibs
        getglobal' "package.loaded.math"
        ltype top
    ]
  , testGroup "setglobbal'"
    [ "sets a global" =:
      Just "new value" `shouldBeResultOf` do
        pushstring "new value"
        setglobal' "new"
        getglobal "new"
        tostring top

    , "can change nested values" =:
      Just "euler" `shouldBeResultOf` do
        openlibs
        pushstring "euler"
        setglobal' "math.test"
        getglobal "math"
        getfield top "test"
        tostring top

    , "can modify 3rd level fields" =:
      Just 42 `shouldBeResultOf` do
        openlibs
        pushinteger 42
        setglobal' "package.loaded.math.foo"
        getglobal "package"
        getfield top "loaded"
        getfield top "math"
        getfield top "foo"
        tointeger top
    ]
  ]
