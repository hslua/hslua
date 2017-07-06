{-
Copyright © 2017 Albert Krewinkel

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
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  Foreign.Lua.ApiTest
Copyright   :  © 2017 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for lua C API-like functions
-}
module Foreign.Lua.ApiTest (tests) where

import Prelude hiding (compare)

import Foreign.Lua
import Test.HsLua.Util (luaTestCase, pushLuaExpr)
import Test.QuickCheck (Property, (.&&.))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Haskell version of the C API"
  [ testGroup "copy"
    [ luaTestCase "copies stack elements using positive indices" $ do
        pushLuaExpr "5, 4, 3, 2, 1"
        copy 4 3
        rawequal 4 3
    , luaTestCase "copies stack elements using negative indices" $ do
        pushLuaExpr "5, 4, 3, 2, 1"
        copy (-1) (-3)
        rawequal (-1) (-3)
    ]

  , testGroup "insert" $
    [ luaTestCase "inserts stack elements using negative indices" $ do
        pushLuaExpr "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert (-6)
        movedEl <- peek (-6) :: Lua LuaInteger
        newTop <- peek (-1) :: Lua LuaInteger
        return (movedEl == 9 && newTop == 8)
    , luaTestCase "inserts stack elements using negative indices" $ do
        pushLuaExpr "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert (4)
        movedEl <- peek 4 :: Lua LuaInteger
        newTop <- peek (-1) :: Lua LuaInteger
        return (movedEl == 9 && newTop == 8)
    ]

  , luaTestCase "gettable gets a table value and returns its type" $ do
      pushLuaExpr "{sum = 13.37}"
      pushnumber 13.37
      pushstring "sum"
      retType <- gettable (-3)
      res <- equal (-1) (-2)
      return (res && retType == TNUMBER)

  , luaTestCase "strlen, objlen, and rawlen all behave the same" $ do
      pushLuaExpr "{1, 1, 2, 3, 5, 8}"
      rlen <- rawlen (-1)
      olen <- objlen (-1)
      slen <- strlen (-1)
      return $ rlen == olen && rlen == slen && rlen == 6

  , luaTestCase "isfunction" $ do
      pushLuaExpr "function () print \"hi!\" end"
      isfunction (-1)

  , luaTestCase "isnil" $ pushLuaExpr "nil" *> isnil (-1)
  , luaTestCase "isnone" $ isnone 500 -- stack index 500 does not exist
  , luaTestCase "isnoneornil" $ do
      pushLuaExpr "nil"
      (&&) <$> isnoneornil 500 <*> isnoneornil (-1)

  , luaTestCase "setting and getting a global works" $ do
      pushLuaExpr "{'Moin', Hello = 'World'}"
      setglobal "hamburg"

      -- get first field
      getglobal "hamburg"
      rawgeti (-1) 1 -- first field
      pushLuaExpr "'Moin'"
      equal (-1) (-2)

  , testGroup "compare"
    [ testProperty "identifies strictly smaller values" $ compareWith (<) OpLT
    , testProperty "identifies smaller or equal values" $ compareWith (<=) OpLE
    , testProperty "identifies equal values" $ compareWith (==) OpEQ
    ]

  , testProperty "lessthan works" $ \n1 n2 -> monadicIO $ do
      luaCmp <- run . runLua $ do
        push (n2 :: LuaNumber)
        push (n1 :: LuaNumber)
        lessthan (-1) (-2) <* pop 2
      assert $ luaCmp == (n1 < n2)
  ]

compareWith :: (Int -> Int -> Bool) -> LuaComparerOp -> Int -> Property
compareWith op luaOp n = compareLT .&&. compareEQ .&&. compareGT
 where
  compareLT :: Property
  compareLT = monadicIO  $ do
    luaCmp <- run . runLua $ do
      push $ n - 1
      push n
      compare (-2) (-1) luaOp
    assert $ luaCmp == op (n - 1) n

  compareEQ :: Property
  compareEQ = monadicIO  $ do
    luaCmp <- run . runLua $ do
      push n
      push n
      compare (-2) (-1) luaOp
    assert $ luaCmp == op n n

  compareGT :: Property
  compareGT = monadicIO $ do
    luaRes <- run . runLua $ do
      push $ n + 1
      push n
      compare (-2) (-1) luaOp
    assert $ luaRes == op (n + 1) n
