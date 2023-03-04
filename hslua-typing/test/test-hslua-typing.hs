{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Main
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for type specifiers.
-}
import HsLua.Typing
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

-- | Run this spec.
main :: IO ()
main = defaultMain tests

-- | Aeson tests
tests :: TestTree
tests = testGroup "hslua-typespec"
  [ testGroup "string representation"
    [ testCase "any"      $ "any"      @?= anyType
    , testCase "boolean"  $ "boolean"  @?= booleanType
    , testCase "function" $ "function" @?= functionType
    , testCase "integer"  $ "integer"  @?= numberType
    , testCase "nil"      $ "nil"      @?= nilType
    , testCase "number"   $ "number"   @?= numberType
    , testCase "string"   $ "string"   @?= stringType
    , testCase "table"    $ "table"    @?= tableType
    , testCase "userdata" $ "userdata" @?= userdataType
    ]

  , testGroup "to string"
    [ testCase "any"      $ typeSpecToString anyType      @?= "any"
    , testCase "boolean"  $ typeSpecToString booleanType  @?= "boolean"
    , testCase "function" $ typeSpecToString functionType @?= "function"
    , testCase "number"   $ typeSpecToString numberType   @?= "number"
    , testCase "string"   $ typeSpecToString stringType   @?= "string"
    , testCase "table"    $ typeSpecToString tableType    @?= "table"
    , testCase "userdata" $ typeSpecToString userdataType @?= "userdata"
    ]
  ]
