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
import Foreign.Lua.Functions
import Foreign.Lua.Interop (push)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)


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
  ]
