{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.ModuleTests
Copyright   : © 2019-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests creating and loading of modules with Haskell.
-}
module Foreign.Lua.ModuleTests (tests) where

import Foreign.Lua (Lua)
import Foreign.Lua.Module (addfield, addfunction, create, preloadhs, requirehs)
import Test.HsLua.Util ((=:), pushLuaExpr, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Module"
  [ testGroup "requirehs"
    [ "pushes module to stack" =:
      1 `shouldBeResultOf` do
        Lua.openlibs
        old <- Lua.gettop
        requirehs "foo" (Lua.pushnumber 5.0)
        new <- Lua.gettop
        return (new - old)

    , "module can be loaded with `require`" =:
      let testModule = "string as a module" :: String
      in testModule `shouldBeResultOf` do
        Lua.openlibs
        requirehs "test.module" (Lua.push testModule)
        pushLuaExpr "require 'test.module'"
        Lua.peek Lua.stackTop
    ]

  , testGroup "preloadhs"
    [ "does not modify the stack" =:
      0 `shouldBeResultOf` do
        Lua.openlibs
        old <- Lua.gettop
        preloadhs "foo" (1 <$ Lua.pushnumber 5.0)
        new <- Lua.gettop
        return (new - old)

    , "module can be loaded with `require`" =:
      let testModule = "string as a module" :: String
      in testModule `shouldBeResultOf` do
        Lua.openlibs
        preloadhs "test.module" (1 <$ Lua.push testModule)
        pushLuaExpr "require 'test.module'"
        Lua.peek Lua.stackTop
    ]

  , testGroup "creation helpers"
    [ "create produces a table" =:
      Lua.TypeTable `shouldBeResultOf` do
        create
        Lua.ltype Lua.stackTop

    , "addfield modifies table" =:
      Lua.Integer 23 `shouldBeResultOf` do
        create
        addfield "field_name" (23 :: Int)
        Lua.getfield Lua.stackTop "field_name"
        Lua.peek Lua.stackTop

    , "addfunction modifies table" =:
      Lua.Integer 5 `shouldBeResultOf` do
        create
        addfunction "minus18" (return . subtract 18 :: Int -> Lua Int)
        Lua.getfield Lua.stackTop "minus18"
        Lua.pushinteger 23
        Lua.call 1 1
        Lua.peek Lua.stackTop
    ]
  ]
