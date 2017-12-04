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
module Foreign.Lua.FunctionCallingTest (tests) where

import Data.ByteString.Char8 (pack, unpack)
import Foreign.Lua.Api
import Foreign.Lua.FunctionCalling (callFunc, peek, registerHaskellFunction,
                                    pushHaskellFunction)
import Foreign.Lua.Types (Lua, LuaException(..), catchLuaError, throwLuaError)
import Foreign.Lua.Util (runLua, runLuaEither)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Interoperability"
  [ testGroup "call haskell functions from lua" $
    let integerOperation :: LuaInteger -> LuaInteger -> Lua LuaInteger
        integerOperation i1 i2 =
          let (j1, j2) = (fromIntegral i1, fromIntegral i2)
          in return $ fromIntegral (product [1..j1] `mod` j2 :: Integer)
    in
    [ testCase "push haskell function to lua" $
      let add :: Lua LuaInteger
          add = do
            i1 <- peek (-1)
            i2 <- peek (-2)
            return (i1 + i2)

          luaOp :: Lua LuaInteger
          luaOp = do
            registerHaskellFunction "add" add
            loadstring "return add(23, 5)" *> call 0 1
            peek (-1) <* pop 1

      in assertEqual "Unexpected result" 28 =<< runLua luaOp

    , testCase "push multi-argument haskell function to lua" $
      let luaOp :: Lua LuaInteger
          luaOp = do
            registerHaskellFunction "integerOp" integerOperation
            loadstring "return integerOp(23, 42)" *> call 0 1
            peek (-1) <* pop 1
      in assertEqual "Unexpected result" 0 =<< runLua luaOp

    , testCase "argument type errors are propagated" $
      let luaOp :: Lua String
          luaOp = do
            registerHaskellFunction "integerOp" integerOperation
            loadstring "return integerOp(23, true)" *> call 0 2
            err <- tostring (-1) <* pop 2 -- pop _HASKELLERROR
            return (unpack err)

          errMsg = "Error during function call: could not read "
                   ++ "argument 2: Expected a number but got a boolean"
      in assertEqual "Unexpected result" errMsg =<< runLua (catchLuaError luaOp (return . show))

    , testCase "convert haskell function to c function" $
      let luaOp :: Lua LuaInteger
          luaOp = do
            pushHaskellFunction integerOperation
            wrapHaskellFunction
            pushinteger 71
            pushinteger 107
            call 2 1
            peek (-1) <* pop 1
      in assertEqual "Unexpected result" 100 =<< runLua luaOp

    , testCase "Error in Haskell function is converted into Lua error" $
      let luaOp :: Lua (Bool, String)
          luaOp = do
            openlibs
            pushHaskellFunction (throwLuaError "foo" :: Lua ())
            wrapHaskellFunction
            setglobal "throw_foo"
            loadstring "return pcall(throw_foo)" *> call 0 2
            (,) <$> peek (-2) <*> peek (-1)

          errMsg = "Error during function call: foo"
      in assertEqual "Unexpected result" (False, errMsg) =<< runLua luaOp
    ]

  , testGroup "call lua function from haskell"
    [ testCase "test equality within lua" $
      assertEqual "raw equality test failed" True =<<
      runLua (openlibs *> callFunc "rawequal" (5 :: LuaInteger) (5.0 :: LuaNumber))

    , testCase "failing lua function call" $
      assertEqual "unexpected result" "foo" =<<
      runLua (catchLuaError (openlibs *> callFunc "assert" False (pack "foo")) (return . show))

    , testCase "print the empty string via lua procedure" $
      assertEqual "raw equality test failed" () =<<
      runLua (openlibs *> callFunc "print" (pack ""))

    , testCase "failing lua procedure call" $
      assertEqual "unexpected result" "foo" =<<
      runLua (catchLuaError (openlibs *> callFunc "error" (pack "foo")) (return . show))
    ]
  ]
