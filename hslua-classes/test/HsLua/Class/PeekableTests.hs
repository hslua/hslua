{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      :  HsLua.Class.PeekableTests
Copyright   :  © 2017-2023 Albert Krewinkel
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

  , testGroup "error handling"
    [ "error is thrown if boolean is given instead of stringy value" =:
      "string expected, got boolean" `shouldBeErrorMessageOf` do
        pushboolean False
        peek top :: Lua ByteString

    , "floating point numbers cannot be peeked as integer" =:
      "integer expected, got number" `shouldBeErrorMessageOf` do
        pushnumber 23.1
        peek top :: Lua Lua.Integer

    , "booleans cannot be retrieved as numbers" =:
      "number expected, got boolean" `shouldBeErrorMessageOf` do
        pushboolean False
        peek top :: Lua Lua.Number

    , "list cannot be read if a peeking at list element fails" =:
      ("number expected, got boolean" ++
       "\n\twhile retrieving index 4" ++
       "\n\twhile retrieving list")
      `shouldBeErrorMessageOf` do
        pushLuaExpr "{1, 5, 23, true, 42}"
        peek top :: Lua [Lua.Number]

    , "stack is unchanged if getting a list fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{true, 1, 1, 2, 3, 5, 8}"
        topBefore <- gettop
        _ <- peek top :: Lua [Bool]
        topAfter <- gettop
        return (topAfter - topBefore)

    , "stack is unchanged if getting key-value pairs fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', baz = false}"
        topBefore <- gettop
        _ <- try (peek top :: Lua [(String, String)])
        topAfter <- gettop
        return (topAfter - topBefore)
    ]
  ]
