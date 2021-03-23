{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      :  HsLua.Class.PeekableTests
Copyright   :  © 2017-2021 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Test for the conversion of lua values to haskell values.
-}
module HsLua.Class.PeekableTests (tests) where

import Data.ByteString (ByteString)
import HsLua.Class.Peekable
import HsLua.Core as Lua
import Test.Tasty.HsLua ( (=:), (?:), pushLuaExpr, shouldBeResultOf
                       , shouldBeErrorMessageOf )
import Test.Tasty (TestTree, testGroup)

import qualified Data.Set as Set

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Peekable"
  [ testGroup "Bool"
    ["literal true is truthy" ?: do
        pushLuaExpr @Lua.Exception "true"
        peek top

    , "0 as a non-nil value is truthy" ?: do
        pushnumber @Lua.Exception 0
        peek top

    , "nil is falsy" ?: do
        pushnil @Lua.Exception
        not <$> peek top
    ]

  , testGroup "Lua.Integer"
    [ "integer can be peeked" =:
      (5 :: Lua.Integer) `shouldBeResultOf` do
        pushnumber 5.0
        peek top
    ]

  , testGroup "Prelude.Integer"
    [ "small integer can be peeked" =:
      (23 :: Prelude.Integer) `shouldBeResultOf` do
        pushnumber 23
        peek top

    , "very large integer can be peeked" =:
      (10000000000000000000001 :: Prelude.Integer) `shouldBeResultOf` do
        pushstring "10000000000000000000001"
        peek top
    ]

  , testGroup "peekKeyValuePairs"
    [ "`next` is not confused when peeking at number keys as strings" =:
      -- list of numbers can be retrieved as pair of strings
      [("1", "2"), ("2", "4"), ("3", "8"), ("4", "16")] `shouldBeResultOf` do
        pushLuaExpr "{2, 4, 8, 16}"
        peekKeyValuePairs top :: Lua [(String, String)]

    , "peek string pairs" =:
      Set.fromList [("foo", "bar"), ("qux", "quux")] `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', qux = 'quux'}"
        Set.fromList <$> (peekKeyValuePairs top :: Lua [(String, String)])

    , "stack is left unchanged" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', qux = 'quux'}"
        topBefore <- gettop
        _ <- peekKeyValuePairs top :: Lua [(String, String)]
        topAfter <- gettop
        return (topAfter - topBefore)
    ]

  , testGroup "error handling"
    [ "error is thrown if boolean is given instead of stringy value" =:
      "expected string, got 'false' (boolean)" `shouldBeErrorMessageOf` do
        pushboolean False
        peek top :: Lua ByteString

    , "floating point numbers cannot be peeked as integer" =:
      "expected integer, got '23.1' (number)" `shouldBeErrorMessageOf` do
        pushnumber 23.1
        peek top :: Lua Lua.Integer

    , "booleans cannot be retrieved as numbers" =:
      "expected number, got 'false' (boolean)" `shouldBeErrorMessageOf` do
        pushboolean False
        peek top :: Lua Lua.Number

    , "list cannot be read if a peeking at list element fails" =:
      "Could not read list: expected number, got 'true' (boolean)"
      `shouldBeErrorMessageOf` do
        pushLuaExpr "{1, 5, 23, true, 42}"
        peek top :: Lua [Lua.Number]

    , "stack is unchanged if getting a list fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{true, 1, 1, 2, 3, 5, 8}"
        topBefore <- gettop
        _ <- peekList top :: Lua [Bool]
        topAfter <- gettop
        return (topAfter - topBefore)

    , "stack is unchanged if getting key-value pairs fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', baz = false}"
        topBefore <- gettop
        _ <- try (peekKeyValuePairs top :: Lua [(String, String)])
        topAfter <- gettop
        return (topAfter - topBefore)
    ]
  ]
