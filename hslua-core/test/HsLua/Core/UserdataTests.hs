{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.UserdataTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that any data type can be pushed to Lua.
-}
module HsLua.Core.UserdataTests (tests) where

import HsLua.Core (getfield, pushboolean, setmetatable, tostring)
import HsLua.Core.Userdata
  (fromuserdata, newhsuserdata, newudmetatable, putuserdata)
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
      fromuserdata top "Sample"

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

  , "change wrapped value" =:
    Just (Sample 1 "a") `shouldBeResultOf` do
      newhsuserdata (Sample 5 "five")
      newudmetatable "Sample"
      setmetatable (nth 2)
      True <- putuserdata top "Sample" (Sample 1 "a")
      fromuserdata top "Sample"

  , "change fails on wrong name" =:
    Just (Sample 2 "b") `shouldBeResultOf` do
      newhsuserdata (Sample 2 "b")
      newudmetatable "Sample"
      setmetatable (nth 2)
      False <- putuserdata top "WRONG" (Sample 3 "c")
      fromuserdata top "Sample"
  ]

-- | Sample data type.
data Sample = Sample Int String
  deriving (Eq, Show)
