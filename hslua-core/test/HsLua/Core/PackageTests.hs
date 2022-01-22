{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.RunTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for different convenience functions to run Lua operations.
-}
module HsLua.Core.PackageTests (tests) where

import HsLua.Core as Lua
import Test.Tasty.HsLua ((=:), pushLuaExpr, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Package"
  [ testGroup "requirehs"
    [ "call the given function" =:
      "test" `shouldBeResultOf` do
        Lua.openlibs
        let openf (Lua.Name name) = Lua.pushstring name
        Lua.requirehs "test" openf
        Lua.tostring' Lua.top

    , "doesn't call function if module has been loaded" =:
      "foo" `shouldBeResultOf` do
        Lua.openlibs
        Lua.requirehs "test" (const $ Lua.pushstring "foo")
        Lua.pop 1
        Lua.requirehs "test" (const $ Lua.failLua "nope")
        Lua.tostring' Lua.top

    , "pushes module to stack" =:
      1 `shouldBeResultOf` do
        Lua.openlibs
        old <- Lua.gettop
        Lua.requirehs "foo" (\_ -> Lua.pushnumber 5.0 *> pushboolean True)
        new <- Lua.gettop
        return (new - old)

    , "module can be loaded with `require`" =:
      let testModule = "string as a module"
      in Just testModule `shouldBeResultOf` do
        Lua.openlibs
        Lua.requirehs "test.module" (const (Lua.pushstring testModule))
        pushLuaExpr "require 'test.module'"
        Lua.tostring Lua.top

    ]

  , testGroup "preloadhs"
    [ "does not modify the stack" =:
      0 `shouldBeResultOf` do
        Lua.openlibs
        old <- Lua.gettop
        Lua.preloadhs "foo" (1 <$ Lua.pushnumber 5.0)
        new <- Lua.gettop
        return (new - old)

    , "module can be loaded with `require`" =:
      let testModule = "string as a module"
      in Just testModule `shouldBeResultOf` do
        Lua.openlibs
        Lua.preloadhs "test.module" (1 <$ Lua.pushstring testModule)
        pushLuaExpr "require 'test.module'"
        Lua.tostring Lua.top
    ]

  ]
