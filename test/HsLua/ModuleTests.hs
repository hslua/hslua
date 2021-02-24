{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.ModuleTests
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests creating and loading of modules with Haskell.
-}
module HsLua.ModuleTests (tests) where

import HsLua (Lua)
import HsLua.Call hiding (render)
import HsLua.Module
import HsLua.Peek (peekIntegral)
import HsLua.Push (pushIntegral)
import Test.Tasty.HsLua ((=:), pushLuaExpr, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Text as T
import qualified HsLua as Lua

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
        Lua.peek Lua.top
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
        Lua.peek Lua.top
    ]

  , testGroup "creation helpers"
    [ "create produces a table" =:
      Lua.TypeTable `shouldBeResultOf` do
        create
        Lua.ltype Lua.top

    , "addfield modifies table" =:
      Lua.Integer 23 `shouldBeResultOf` do
        create
        addfield "field_name" (23 :: Int)
        Lua.getfield Lua.top "field_name"
        Lua.peek Lua.top

    , "addfunction modifies table" =:
      Lua.Integer 5 `shouldBeResultOf` do
        create
        addfunction "minus18" (return . subtract 18 :: Int -> Lua Int)
        Lua.getfield Lua.top "minus18"
        Lua.pushinteger 23
        Lua.call 1 1
        Lua.peek Lua.top
    ]
  , testGroup "module type"
    [ "register module" =:
      1 `shouldBeResultOf` do
        Lua.openlibs
        old <- Lua.gettop
        registerModule mymath
        new <- Lua.gettop
        return (new - old)

    , "call module function" =:
      Right 24 `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        _ <- Lua.dostring $ mconcat
             [ "local mymath = require 'mymath'\n"
             , "return mymath.factorial(4)"
             ]
        peekIntegral @Integer Lua.top
    ]
  , testGroup "documentation"
    [ "module docs" =:
      (T.intercalate "\n"
        [ "# mymath"
        , ""
        , "A math module."
        , ""
        , "## Functions"
        , ""
        , "### factorial (n)"
        , ""
        , "Parameters:"
        , ""
        , "n"
        , ":   number for which the factorial is computed (integer)"
        , ""
        , "Returns:"
        , ""
        , " - factorial (integer)\n"
        ] @=?
        render mymath)
    ]
  ]

mymath :: Module
mymath = Module
  { moduleName = "mymath"
  , moduleDescription = "A math module."
  , moduleFields = []
  , moduleFunctions = [ ("factorial", factorial)]
  }

factorial :: HaskellFunction
factorial = toHsFnPrecursor (\n -> product [1..n])
  <#> factorialParam
  =#> factorialResult

factorialParam :: Parameter Integer
factorialParam =
  parameter (peekIntegral @Integer) "integer"
    "n"
    "number for which the factorial is computed"

factorialResult :: FunctionResults Integer
factorialResult =
  functionResult (pushIntegral @Integer) "integer" "factorial"
