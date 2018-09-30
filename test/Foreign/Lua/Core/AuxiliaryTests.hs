{-# LANGUAGE OverloadedStrings #-}
{-| Tests for the auxiliary library.
-}
module Foreign.Lua.Core.AuxiliaryTests (tests) where

import Test.HsLua.Util ((?:), (=:), pushLuaExpr, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Auxiliary"
  [ testGroup "getsubtable"
    [ "gets a subtable from field" =:
      [1, 2, 3, 5, 8] `shouldBeResultOf` do
        pushLuaExpr "{foo = {1, 2, 3, 5, 8}}"
        _ <- Lua.getsubtable Lua.stackTop "foo"
        Lua.peek Lua.stackTop :: Lua.Lua [Int]

    , "creates new table at field if necessary" =:
      Lua.TypeTable `shouldBeResultOf` do
        Lua.newtable
        _ <- Lua.getsubtable Lua.stackTop "new"
        Lua.getfield (Lua.nthFromTop 2) "new"
        Lua.ltype Lua.stackTop

    , "returns True if a table exists" ?: do
        pushLuaExpr "{yep = {}}"
        Lua.getsubtable Lua.stackTop "yep"

    , "returns False if field does not contain a table" ?: do
        pushLuaExpr "{nope = 5}"
        not <$> Lua.getsubtable Lua.stackTop "nope"

    ]

  , testGroup "getmetatable'"
    [ "gets table created with newmetatable" =:
      [("__name" :: String, "testing" :: String)] `shouldBeResultOf` do
        Lua.newmetatable "testing" *> Lua.pop 1
        _ <- Lua.getmetatable' "testing"
        Lua.peekKeyValuePairs Lua.stackTop

    , "returns nil if there is no such metatable" =:
      Lua.TypeNil `shouldBeResultOf` do
        _ <- Lua.getmetatable' "nope"
        Lua.ltype Lua.stackTop

    , "returns TypeTable if metatable exists" =:
      Lua.TypeTable `shouldBeResultOf` do
        _ <- Lua.newmetatable "yep"
        Lua.getmetatable' "yep"
    ]

  , "loadedTable" =: ("_LOADED" @=? Lua.loadedTableRegistryField)
  , "preloadTable" =: ("_PRELOAD" @=? Lua.preloadTableRegistryField)
  ]
