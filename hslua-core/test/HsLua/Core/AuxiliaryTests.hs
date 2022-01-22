{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| Tests for the auxiliary library.
-}
module HsLua.Core.AuxiliaryTests (tests) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import HsLua.Core (nth)
import Test.Tasty.HsLua ( (?:), (=:), pushLuaExpr, shouldBeResultOf
                        , shouldBeErrorMessageOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Lua
import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Auxiliary"
  [ testGroup "checkstack'"
    [ "returns unit if stack space can be provided" =:
      () `shouldBeResultOf` Lua.checkstack' 2 "test"

      -- testing the error case is not possible for some reason
      -- , "fails if too much stack space is requested" =:
      --    "stack overflow (test)"`shouldBeErrorMessageOf`
      --    Lua.checkstack' maxBound "test"
    ]

  , testGroup "getsubtable"
    [ "gets a subtable from field" =:
      [5, 8] `shouldBeResultOf` do
        pushLuaExpr @Lua.Exception "{foo = {5, 8}}"
        _ <- Lua.getsubtable Lua.top "foo"
        Lua.rawgeti (nth 1) 1
        Lua.rawgeti (nth 2) 2
        i1 <- fromMaybe 0 <$> Lua.tointeger (nth 2)
        i2 <- fromMaybe 0 <$> Lua.tointeger (nth 1)
        return [i1, i2]

    , "creates new table at field if necessary" =:
      Lua.TypeTable `shouldBeResultOf` do
        Lua.newtable
        _ <- Lua.getsubtable Lua.top "new"
        Lua.getfield (Lua.nth 2) "new"
        Lua.ltype Lua.top

    , "returns True if a table exists" ?: do
        pushLuaExpr @Lua.Exception "{yep = {}}"
        Lua.getsubtable Lua.top "yep"

    , "returns False if field does not contain a table" ?: do
        pushLuaExpr @Lua.Exception "{nope = 5}"
        not <$> Lua.getsubtable Lua.top "nope"

    ]

  , testGroup "getmetafield'"
    [ "gets field from the object's metatable" =:
      ("testing" :: ByteString) `shouldBeResultOf` do
        Lua.newtable
        pushLuaExpr "{foo = 'testing'}"
        Lua.setmetatable (Lua.nth 2)
        _ <- Lua.getmetafield Lua.top "foo"
        Lua.tostring' Lua.top

    , "returns TypeNil if the object doesn't have a metatable" =:
      Lua.TypeNil `shouldBeResultOf` do
        Lua.newtable
        Lua.getmetafield Lua.top "foo"
    ]

  , testGroup "getmetatable'"
    [ "gets table created with newmetatable" =:
      ("__name" :: ByteString, "testing" :: ByteString) `shouldBeResultOf` do
        Lua.newmetatable "testing" *> Lua.pop 1
        _ <- Lua.getmetatable' "testing"
        Lua.pushnil
        Lua.next (nth 2)
        key <- Lua.tostring' (nth 2) <* Lua.pop 1
        value <- Lua.tostring' (nth 1) <* Lua.pop 1
        return (key, value)

    , "returns nil if there is no such metatable" =:
      Lua.TypeNil `shouldBeResultOf` do
        _ <- Lua.getmetatable' "nope"
        Lua.ltype Lua.top

    , "returns TypeTable if metatable exists" =:
      Lua.TypeTable `shouldBeResultOf` do
        _ <- Lua.newmetatable "yep"
        Lua.getmetatable' "yep"
    ]

  , testGroup "requiref"
    [ "can load a module" =: do
        Lua.TypeTable `shouldBeResultOf` do
          Lua.openlibs
          Lua.requiref "mathematics" Lua.luaopen_math False
          Lua.ltype Lua.top

    , "returns () on success" =: do
        () `shouldBeResultOf` do
          Lua.openlibs
          -- already loaded
          Lua.requiref "math" Lua.luaopen_math False

    , "sets global if flag is set" =: do
        Lua.TypeTable `shouldBeResultOf` do
          Lua.openlibs
          Lua.requiref "foo" Lua.luaopen_math True
          Lua.pop 1
          Lua.getglobal "foo"

    , "catches errors" =: do
        "attempt to index a nil value" `shouldBeErrorMessageOf` do
          -- unset registry
          Lua.pushnil
          Lua.copy Lua.top Lua.registryindex
          Lua.requiref "math" Lua.luaopen_package False
    ]

  , testGroup "where'"
    [ "return location in chunk" =:
      "test:1: nope, not yet" `shouldBeResultOf` do
        Lua.openlibs
        Lua.pushHaskellFunction $ 1 <$ do
          Lua.settop 1
          Lua.where' 2
          Lua.pushstring "nope, "
          Lua.pushvalue 1
          Lua.concat 3
        Lua.setglobal "frob"
        Lua.OK <- Lua.loadbuffer
          "return frob('not yet')"
          "@test"
        result <- Lua.pcall 0 1 Nothing
        if result /= Lua.OK
          then Lua.throwErrorAsException
          else Lua.tostring' Lua.top
    ]

  , "loaded" =: ("_LOADED" @=? Lua.loaded)
  , "preload" =: ("_PRELOAD" @=? Lua.preload)
  ]
