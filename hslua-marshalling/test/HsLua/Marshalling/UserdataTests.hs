
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Marshalling.UserdataTests
Copyright   : © 2018-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that any data type can be pushed to Lua as userdata.
-}
module HsLua.Marshalling.UserdataTests (tests) where

import Control.Monad (when)
import Data.Data (Data)
import Data.Word (Word64)
import Data.Typeable (Typeable)
import HsLua.Marshalling.Userdata
import Test.Tasty.HsLua ( (=:), shouldBeResultOf, shouldHoldForResultOf )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual)

import qualified Data.ByteString as B
import qualified HsLua.Core as Lua

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

  , testGroup "roundtrip"
    [ "roundtrip dummy" =:
      Just (Dummy 42 "answer") `shouldBeResultOf` do
        pushAny (Dummy 42 "answer")
        toAny Lua.top
    ]

  , testGroup "pushIterator"
    [ "iterate over list" =:
      Just "0,1,1,2,3,5,8,13,21" `shouldBeResultOf` do
        let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
        Lua.openlibs
        Lua.pushHaskellFunction $
          pushIterator (\n -> 1 <$ Lua.pushinteger n) (take 9 fibs)
        Lua.setglobal "fibs"
        stat <- Lua.dostring $ mconcat
          [ "local acc = {}\n"
          , "for n in fibs() do\n"
          , "  table.insert(acc, n)\n"
          , "end\n"
          , "return table.concat(acc, ',')\n"
          ]
        when (stat /= Lua.OK) Lua.throwErrorAsException
        Lua.tostring Lua.top
    ]
  ]

-- | Dummy data
data Dummy = Dummy Int String
  deriving (Data, Eq, Show, Typeable)
