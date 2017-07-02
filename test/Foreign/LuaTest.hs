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
{-| Tests for lua -}
module Foreign.LuaTest (tests) where

import Prelude hiding (concat)

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Foreign.Lua
import Test.HsLua.Util (pushLuaExpr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assert)


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua integration tests"
  [ testCase "print version" .
    runLua $ do
      openlibs
      getglobal "assert"
      push ("Hello from " :: ByteString)
      getglobal "_VERSION"
      concat 2
      call 1 0

  , testCase "functions stored in / retrieved from registry" .
    runLua $ do
      pushLuaExpr "function() return 2 end, function() return 1 end"
      idx1 <- ref registryindex
      idx2 <- ref registryindex
      -- functions are removed from stack
      liftIO . assert =<< fmap (TFUNCTION /=) (ltype (-1))

      -- get functions from registry
      rawgeti registryindex idx1
      call 0 1
      r1 <- peek (-1) :: Lua (Result LuaInteger)
      liftIO (assert (r1 == Success 1))

      rawgeti registryindex idx2
      call 0 1
      r2 <- peek (-1) :: Lua (Result LuaInteger)
      liftIO (assert (r2 == Success 2))

      -- delete references
      unref registryindex idx1
      unref registryindex idx2

  , testCase "table reading" .
    runLua $ do
      openbase
      let tableStr = "{firstname = 'Jane', surname = 'Doe'}"
      pushLuaExpr $ "setmetatable(" <> tableStr <> ", {'yup'})"
      getfield (-1) "firstname"
      firstname <- peek (-1) <* pop 1
      liftIO (assert (firstname == Success ("Jane" :: ByteString)))

      push ("surname" :: ByteString)
      rawget (-2)
      surname <- peek (-1) <* pop 1
      liftIO (assert (surname == Success ("Doe" :: ByteString)))

      hasMetaTable <- getmetatable (-1)
      liftIO (assert hasMetaTable)
      rawgeti (-1) 1
      mt1 <- peek (-1) <* pop 1
      liftIO (assert (mt1 == Success ("yup" :: ByteString)))
  ]
