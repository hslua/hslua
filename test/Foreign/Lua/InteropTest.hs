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
-- | Tests that lua functions can be called from haskell and vice versa.
module Foreign.Lua.InteropTest (tests) where

import Data.ByteString.Char8 (pack)
import Foreign.Lua.Functions
import Foreign.Lua.Interop (callfunc, peek, registerhsfunction)
import Foreign.Lua.Types (Lua, LuaNumber, Result (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Interoperability"
  [ testGroup "call haskell functions from lua"
    [ testCase "push haskell function to lua" $
      let add = do
            i1 <- peek (-1)
            i2 <- peek (-2)
            case (+) <$> i1 <*> i2 of
              Success x -> return x
              Error _ -> lerror
          luaOp = do
            registerhsfunction "add" add
            loadstring "return add(23, 5) == 28" *> call 0 1
            res <- peek (-1)
            return $ res == Success True
      in assertBool "Operation was successful" =<< runLua luaOp

    , testCase "push multi-argument haskell function to lua" $
      let
        integerOperation :: Int -> Int -> Lua Int
        integerOperation i1 i2 =
          let (j1, j2) = (fromIntegral i1, fromIntegral i2)
          in return $ fromIntegral (product [1..j1] `mod` j2 :: Integer)
        luaOp = do
          registerhsfunction "integerOp" integerOperation
          loadstring "return integerOp(23, 42) == 0" *> call 0 1
          res <- peek (-1)
          return $ res == Success True
      in assertBool "Operation was successful" =<< runLua luaOp
    ]

  , testGroup "call lua function from haskell"
    [ testCase "test equality within lua" $
      assertEqual "raw equality test failed" (Success True) =<<
      runLua (openlibs *> callfunc "rawequal" (5 :: Int) (5.0 :: LuaNumber))

    , testCase "failing lua function call" $
      assertEqual "unexpected result" (Error "foo" :: Result Int) =<<
      runLua (openlibs *> callfunc "assert" False (pack "foo"))

    , testCase "print the empty string via lua procedure" $
      assertEqual "raw equality test failed" (Success ()) =<<
      runLua (openlibs *> callfunc "print" (pack ""))

    , testCase "failing lua procedure call" $
      assertEqual "unexpected result" (Error "foo" :: Result ()) =<<
      runLua (openlibs *> callfunc "error" (pack "foo"))
    ]
  ]
