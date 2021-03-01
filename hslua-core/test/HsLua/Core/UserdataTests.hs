{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.UserdataTests
Copyright   :  © 2017-2021 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that any data type can be pushed to Lua.
-}
module HsLua.Core.UserdataTests (tests) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import HsLua.Core (getfield, pushboolean, setmetatable, tostring)
import HsLua.Core.Userdata (fromuserdata, newhsuserdata, newudmetatable)
import HsLua.Core.Types (nth, top)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Userdata"
  [ "Name is kept in __name" =:
    Just "Sample" `shouldBeResultOf` do
      newudmetatable "Sample"
      getfield top "__name"
      tostring top

  , "get back pushed value" =:
    Just (Sample 0 "zero") `shouldBeResultOf` do
      newhsuserdata (Sample 0 "zero")
      newudmetatable "Sample"
      setmetatable (nth 2)
      fromuserdata (top) "Sample"

  , "fail on boolean" =:
    (Nothing :: Maybe Sample) `shouldBeResultOf` do
      pushboolean False
      fromuserdata top "Sample"

  , "fail on wrong userdata" =:
    (Nothing :: Maybe Sample) `shouldBeResultOf` do
      newhsuserdata (5 :: Integer)
      newudmetatable "Integer"
      setmetatable (nth 2)
      fromuserdata top "Sample"
  ]

-- | Sample data type.
data Sample = Sample Int String
  deriving (Data, Eq, Show, Typeable)
