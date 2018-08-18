{-
Copyright © 2017-2018 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  Foreign.Lua.Types.PeekableTest
Copyright   :  © 2017-2018 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Test for the conversion of lua values to haskell values.
-}
module Foreign.Lua.Types.PeekableTest (tests) where

import Data.ByteString (ByteString)
import Foreign.Lua as Lua
import Test.HsLua.Util ( (=:), (?:), pushLuaExpr, shouldBeResultOf
                       , shouldBeErrorMessageOf )
import Test.Tasty (TestTree, testGroup)

import qualified Data.Set as Set

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Peekable"
  [ testGroup "Bool"
    ["literal true is truthy" ?: do
        pushLuaExpr "true"
        peek stackTop

    , "0 as a non-nil value is truthy" ?: do
        pushnumber 0
        peek stackTop

    , "nil is falsy" ?: do
        pushnil
        not <$> peek stackTop
    ]

  , testGroup "Lua.Integer"
    [ "integer can be peeked" =:
      (5 :: Lua.Integer) `shouldBeResultOf` do
        pushnumber 5.0
        peek stackTop
    ]

  , testGroup "peekKeyValuePairs"
    [ "`next` is not confused when peeking at number keys as strings" =:
      -- list of numbers can be retrieved as pair of strings
      [("1", "2"), ("2", "4"), ("3", "8"), ("4", "16")] `shouldBeResultOf` do
        pushLuaExpr "{2, 4, 8, 16}"
        peekKeyValuePairs stackTop :: Lua [(String, String)]

    , "peek string pairs" =:
      Set.fromList [("foo", "bar"), ("qux", "quux")] `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', qux = 'quux'}"
        Set.fromList <$> (peekKeyValuePairs stackTop :: Lua [(String, String)])

    , "stack is left unchanged" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', qux = 'quux'}"
        topBefore <- gettop
        _ <- peekKeyValuePairs stackTop :: Lua [(String, String)]
        topAfter <- gettop
        return (topAfter - topBefore)
    ]

  , testGroup "error handling"
    [ "error is thrown if boolean is given instead of stringy value" =:
      "expected string, got 'false' (boolean)" `shouldBeErrorMessageOf` do
        pushboolean False
        peek stackTop :: Lua ByteString

    , "floating point numbers cannot be peeked as integer" =:
      "expected integer, got '23.1' (number)" `shouldBeErrorMessageOf` do
        pushnumber 23.1
        peek stackTop :: Lua Lua.Integer

    , "booleans cannot be retrieved as numbers" =:
      "expected number, got 'false' (boolean)" `shouldBeErrorMessageOf` do
        pushboolean False
        peek stackTop :: Lua Lua.Number

    , "list cannot be read if a peeking at list element fails" =:
      "Could not read list: expected number, got 'true' (boolean)"
      `shouldBeErrorMessageOf` do
        pushLuaExpr "{1, 5, 23, true, 42}"
        peek stackTop :: Lua [Lua.Number]

    , "stack is unchanged if getting a list fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{true, 1, 1, 2, 3, 5, 8}"
        topBefore <- gettop
        _ <- peekList stackTop :: Lua [Bool]
        topAfter <- gettop
        return (topAfter - topBefore)

    , "stack is unchanged if getting key-value pairs fails" =:
      0 `shouldBeResultOf` do
        pushLuaExpr "{foo = 'bar', baz = false}"
        topBefore <- gettop
        _ <- try (peekKeyValuePairs stackTop :: Lua [(String, String)])
        topAfter <- gettop
        return (topAfter - topBefore)
    ]
  ]
