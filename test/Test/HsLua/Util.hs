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
{-| Utilities for testing hslua -}
module Test.HsLua.Util
  ( assertLuaBool
  , pushLuaExpr
  , shouldBeResultOf
  , (=:)
  , (?:)
  ) where

import Foreign.Lua ( Lua, LuaException (..), runLua, runLuaEither, loadstring
                   , call, multret)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

pushLuaExpr :: String -> Lua ()
pushLuaExpr expr = loadstring ("return " ++ expr) *> call 0 multret

shouldBeResultOf :: (Eq a, Show a) => a -> Lua a -> Assertion
shouldBeResultOf expected luaOp = do
  errOrRes <- runLuaEither luaOp
  case errOrRes of
    Left (LuaException msg) -> assertFailure $ "Lua operation failed with "
                               ++ "message: '" ++ msg ++ "'"
    Right res -> res @?= expected

assertLuaBool :: Lua Bool -> Assertion
assertLuaBool luaOp = assertBool "" =<< runLua luaOp

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase

infixr 3 ?:
(?:) :: String -> Lua Bool -> TestTree
(?:) = luaTestBool

luaTestBool :: String -> Lua Bool -> TestTree
luaTestBool msg luaOp = testCase msg $
  assertBool "Lua operation returned false" =<< runLua luaOp
