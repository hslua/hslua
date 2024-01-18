{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.UserdataTests
Copyright   :  © 2017-2024 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb@hslua.org>

Tests that any data type can be pushed to Lua.
-}
module HsLua.Core.UserdataTests (tests) where

import HsLua.Core (getfield, pushboolean, setmetatable, tostring)
import HsLua.Core.Userdata
  (fromuserdata, newhsuserdatauv, newudmetatable, putuserdata)
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
      newhsuserdatauv (Sample 0 "zero") 0
      newudmetatable "Sample"
      setmetatable (nth 2)
      fromuserdata top "Sample"

  , "fail on boolean" =:
    (Nothing :: Maybe Sample) `shouldBeResultOf` do
      pushboolean False
      fromuserdata top "Sample"

  , "fail on wrong userdata" =:
    (Nothing :: Maybe Sample) `shouldBeResultOf` do
      newhsuserdatauv (5 :: Integer) 0
      newudmetatable "Integer"
      setmetatable (nth 2)
      fromuserdata top "Sample"

  , "change wrapped value" =:
    Just (Sample 1 "a") `shouldBeResultOf` do
      newhsuserdatauv (Sample 5 "five") 0
      newudmetatable "Sample"
      setmetatable (nth 2)
      True <- putuserdata top "Sample" (Sample 1 "a")
      fromuserdata top "Sample"

  , "change fails on wrong name" =:
    Just (Sample 2 "b") `shouldBeResultOf` do
      newhsuserdatauv (Sample 2 "b") 0
      newudmetatable "Sample"
      setmetatable (nth 2)
      False <- putuserdata top "WRONG" (Sample 3 "c")
      fromuserdata top "Sample"
  ]

-- | Sample data type.
data Sample = Sample Int String
  deriving (Eq, Show)
