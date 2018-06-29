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
import Foreign.Lua.Core
import Foreign.Lua.Types (Result (Error, Success))
import Foreign.Lua.FunctionCalling (callFunc, peek, registerHaskellFunction,
                                    pushHaskellFunction)
import Foreign.Lua.Util (runLua)
import Test.HsLua.Util ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Data.ByteString.Char8 as Char8

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
            err <- tostring (-1) <* pop 2 -- pop HSLUA_ERR
            return (Char8.unpack err)

          errMsg = "Error during function call: could not read "
                   ++ "argument 2: Expected a integer but got a boolean"
      in assertEqual "Unexpected result" errMsg =<< runLua (catchLuaError luaOp (return . show))

    , "Haskell functions are converted to C functions" =:
      (100 :: LuaInteger) `shouldBeResultOf` do
        pushHaskellFunction integerOperation
        pushinteger 71
        pushinteger 107
        call 2 1
        peek stackTop <* pop 1

    , "Error in Haskell function is converted into Lua error" =:
      (False, "Error during function call: foo" :: String) `shouldBeResultOf` do
        openlibs
        pushHaskellFunction (throwLuaError "foo" :: Lua ())
        setglobal "throw_foo"
        loadstring "return pcall(throw_foo)" *> call 0 2
        (,) <$> peek (nthFromTop 2) <*> peek (nthFromTop 1)
    ]

  , testGroup "call lua function from haskell"
    [ testCase "test equality within lua" $
      assertEqual "raw equality test failed" (Success True) =<<
      runLua (openlibs *> callFunc "rawequal" (5 :: LuaInteger) (5.0 :: LuaNumber))

    , testCase "failing lua function call" $
      assertEqual "unexpected result" (Left (LuaException "foo")) =<<
      let luaOp = do
             openlibs
             callFunc "assert" False (Char8.pack "foo") :: Lua (Result Bool)
      in runLua (tryLua luaOp)

    , testCase "print the empty string via lua procedure" $
      assertEqual "raw equality test failed" (Success ()) =<<
      runLua (openlibs *> callFunc "print" (Char8.pack ""))

    , testCase "failing lua procedure call" $
      assertEqual "unexpected result" (Left (LuaException "foo")) =<<
      let luaOp = (openlibs *> callFunc "error" (Char8.pack "foo") :: Lua (Result ()))
      in runLua (tryLua luaOp)

    , testCase "Error result when Lua-to-Haskell result conversion fails" $ do
        luaRes <- runLua $ do
          openlibs
          callFunc "rawequal" (Char8.pack "a") () :: Lua (Result String)
        let msg = pack "Expected a string but got a boolean"
        assertEqual "raw equality test failed" (Error [msg]) luaRes
    ]

  -- The following test case will hang if there's a problem with garbage
  -- collection.
  , testCase "function garbage collection" $
    assertEqual "problem while pushing many Haskell functions" () =<<
      ( runLua $ forM_ [1..5000::LuaInteger] $ \n -> do
         let fn :: LuaInteger -> Lua LuaInteger
             fn x = return (x + n)
         pushHaskellFunction fn
         pop 1
         gc GCCOLLECT 0
      )
  ]
