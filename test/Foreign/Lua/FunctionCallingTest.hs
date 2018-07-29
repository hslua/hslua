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
-- | Tests that lua functions can be called from haskell and vice versa.
module Foreign.Lua.FunctionCallingTest (tests) where

import Control.Monad (forM_)
import Data.ByteString.Char8 as Char8
import Data.Monoid ((<>))
import Foreign.Lua (Lua)
import Test.HsLua.Util ( (=:), pushLuaExpr, shouldBeErrorMessageOf
                       , shouldBeResultOf )
import Test.Tasty (TestTree, testGroup)

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "FunctionCalling"
  [ testGroup "call haskell functions from lua" $
    let integerOperation :: Lua.Integer -> Lua.Integer -> Lua Lua.Integer
        integerOperation i1 i2 =
          let (j1, j2) = (fromIntegral i1, fromIntegral i2)
          in return $ fromIntegral (product [1..j1] `mod` j2 :: Prelude.Integer)
    in
    [ "push haskell function to lua" =:
      (28 :: Lua.Integer) `shouldBeResultOf` do
        let add :: Lua Lua.Integer
            add = do
              i1 <- Lua.peek (-1)
              i2 <- Lua.peek (-2)
              return (i1 + i2)
        Lua.registerHaskellFunction "add" add
        Lua.loadstring "return add(23, 5)" *> Lua.call 0 1
        Lua.peek Lua.stackTop <* Lua.pop 1

    , "push multi-argument haskell function to lua" =:
      (0 :: Lua.Integer) `shouldBeResultOf` do
        Lua.registerHaskellFunction "integerOp" integerOperation
        Lua.loadstring "return integerOp(23, 42)" *> Lua.call 0 1
        Lua.peek (-1) <* Lua.pop 1

    , "argument type errors are propagated" =:
       ("Error during function call: could not read argument 2: "
        <> "expected integer, got 'true' (boolean)") `shouldBeErrorMessageOf` do
            Lua.registerHaskellFunction "integerOp" integerOperation
            pushLuaExpr "integerOp(23, true)"

    , "Haskell functions are converted to C functions" =:
      (100 :: Lua.Integer) `shouldBeResultOf` do
        Lua.pushHaskellFunction integerOperation
        Lua.pushinteger 71
        Lua.pushinteger 107
        Lua.call 2 1
        Lua.peek Lua.stackTop <* Lua.pop 1

    , "Error in Haskell function is converted into Lua error" =:
      (False, "Error during function call: foo" :: String) `shouldBeResultOf` do
        Lua.openlibs
        Lua.pushHaskellFunction (Lua.throwLuaError "foo" :: Lua ())
        Lua.setglobal "throw_foo"
        Lua.loadstring "return pcall(throw_foo)" *> Lua.call 0 2
        (,) <$> Lua.peek (Lua.nthFromTop 2) <*> Lua.peek (Lua.nthFromTop 1)
    ]

  , testGroup "call lua function from haskell"
    [ "test equality within lua" =:
      True `shouldBeResultOf` do
        Lua.openlibs
        Lua.callFunc "rawequal" (5 :: Lua.Integer) (5.0 :: Lua.Number)

    , "failing lua function call" =:
      "foo" `shouldBeErrorMessageOf` do
        Lua.openlibs
        Lua.callFunc "assert" False (Char8.pack "foo") :: Lua Bool

    , "pack table via lua procedure" =:
      (True, 23 :: Lua.Integer, "moin" :: ByteString) `shouldBeResultOf` do
        Lua.openlibs
        Lua.callFunc "table.pack" True (23 :: Lua.Integer) (Char8.pack "moin")

    , "failing lua procedure call" =:
      "foo" `shouldBeErrorMessageOf` do
        Lua.openlibs
        Lua.callFunc "error" (Char8.pack "foo") :: Lua ()

    , "Error when Lua-to-Haskell result conversion fails" =:
      "expected string, got 'false' (boolean)" `shouldBeErrorMessageOf` do
          Lua.openlibs
          Lua.callFunc "rawequal" (Char8.pack "a") () :: Lua String
    ]

  -- The following test case will hang if there's a problem with garbage
  -- collection.
  , "function garbage collection" =:
    () `shouldBeResultOf` do
      let pushAndPopAdder n = do
            let fn :: Lua.Integer -> Lua Lua.Integer
                fn x = return (x + n)
            Lua.pushHaskellFunction fn
            Lua.pop 1
      forM_ [1..5000::Lua.Integer] pushAndPopAdder
      () <$ Lua.gc Lua.GCCOLLECT 0
  ]
