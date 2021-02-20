{-
Copyright © 2018-2021 Albert Krewinkel

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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests that any data type can be pushed to Lua.
module HsLua.UserdataTests (tests) where

import Data.Data (Data)
import Data.Word (Word64)
import Data.Typeable (Typeable)
import HsLua.Userdata (metatableName, pushAny, peekAny, toAny)
import Test.HsLua.Util ( (=:), shouldBeResultOf, shouldHoldForResultOf )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual)

import qualified Data.ByteString as B
import qualified HsLua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Userdata"
  [ testGroup "metatableName"
    [ "Dummy" =:
      assertEqual "" "HSLUA_Dummy" (metatableName (Dummy 5 "Moin"))

    , "Word64" =:
      assertEqual "" "HSLUA_Data.Word.Word64" (metatableName (0 :: Word64))
    ]

  , testGroup "pushAny"
    [ "metatable is named Dummy" =:
      Just "HSLUA_Dummy" `shouldBeResultOf` do
        pushAny (Dummy 23 "Nichts ist wie es scheint")
        _ <- Lua.getmetatable Lua.top
        Lua.getfield Lua.top "__name"
        Lua.tostring Lua.top

    , "userdata is named Dummy" =:
      ("HSLUA_Dummy" `B.isPrefixOf`) `shouldHoldForResultOf` do
        pushAny (Dummy 23 "Nichts ist wie es scheint")
        Lua.tostring' Lua.top
    ]

  , testGroup "toAny"
    [ "get back pushed value" =:
      Just (Dummy 0 "zero") `shouldBeResultOf` do
        pushAny (Dummy 0 "zero")
        toAny Lua.top

    , "fail on boolean" =:
      (Nothing :: Maybe Dummy) `shouldBeResultOf` do
        Lua.pushboolean False
        toAny Lua.top

    , "fail on wrong userdata" =:
      (Nothing :: Maybe Dummy) `shouldBeResultOf` do
        pushAny (0 :: Word64)
        toAny Lua.top
    ]

  , testGroup "Peekable & Pushable"
    [ "push and peek" =:
      Dummy 5 "sum of digits" `shouldBeResultOf` do
        Lua.push (Dummy 5 "sum of digits")
        Lua.peek Lua.top
    ]

  , testGroup "roundtrip"
    [ "roundtrip dummy" =:
      Just (Dummy 42 "answer") `shouldBeResultOf` do
        pushAny (Dummy 42 "answer")
        toAny Lua.top
    ]
  ]

-- | Dummy data
data Dummy = Dummy Int String
  deriving (Data, Eq, Show, Typeable)

instance Lua.Peekable Dummy where
  peek = peekAny

instance Lua.Pushable Dummy where
  push = pushAny
