{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.ModuleTests
Copyright   : © 2019-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests creating and loading of modules with Haskell.
-}
module HsLua.Packaging.ModuleTests (tests) where

import HsLua.Core
import HsLua.Marshalling
  ( forcePeek, peekFieldRaw, peekIntegral, peekList, peekName, peekString
  , pushIntegral, pushText)
import HsLua.Packaging.Documentation
import HsLua.Packaging.Function
import HsLua.Packaging.Module
import HsLua.Packaging.Types (FieldDoc (..))
import HsLua.Packaging.UDType (deftype, initType)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Module"
  [ testGroup "creation helpers"
    [ "create produces a table" =:
      Lua.TypeTable `shouldBeResultOf` do
        Lua.newtable
        Lua.ltype Lua.top
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
      24 `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        _ <- Lua.dostring $ mconcat
             [ "local mymath = require 'mymath'\n"
             , "return mymath.factorial(4)"
             ]
        forcePeek $ peekIntegral @Prelude.Integer Lua.top

    , "call module as function" =:
      "call me maybe" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        _ <- Lua.dostring "return (require 'mymath')()"
        forcePeek $ peekString Lua.top

    , "access name in docs" =:
      "mymath" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeTable <- getdocumentation top
        forcePeek $ peekFieldRaw peekString "name" Lua.top

    , "first function name in docs" =:
      "factorial" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeTable <- getdocumentation top
        TypeTable <- getfield top "functions"
        TypeTable <- rawgeti top 1
        forcePeek $ peekFieldRaw peekString "name" Lua.top

    , "function doc is shared" =:
      True `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        pushvalue top
        setglobal "mymath"
        -- get doc table via module docs
        TypeTable <- getdocumentation top
        TypeTable <- getfield top "functions"
        TypeTable <- rawgeti top 1
        -- get doc table via function
        OK <- dostring "return mymath.factorial"
        TypeTable <- getdocumentation top
        -- must be the same
        rawequal (nth 1) (nth 3)

    , "first field name in docs" =:
      "unit" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeTable <- getdocumentation top
        TypeTable <- getfield top "fields"
        TypeTable <- rawgeti top 1
        forcePeek $ peekFieldRaw peekString "name" Lua.top

    , "document object has associated types" =:
      ["Void"] `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeTable <- getdocumentation top
        TypeFunction <- getfield top "types"
        call 0 1
        forcePeek $ peekList peekName top
    ]
  ]

mymath :: Module Lua.Exception
mymath = Module
  { moduleName = "mymath"
  , moduleDescription = "A math module."
  , moduleFields = [
      let docs = FieldDoc "unit" "integer" "additive unit"
      in Field "unit" docs (pushinteger 1)
    ]
  , moduleFunctions = [factorial]
  , moduleOperations =
    [ (,) Call $ lambda
      ### (1 <$ pushText "call me maybe")
      =?> "call result"
    ]
  , moduleTypeInitializers = [initType (deftype "Void" [] [])]
  }

factorial :: DocumentedFunction Lua.Exception
factorial =
  defun "factorial"
  ### liftPure (\n -> product [1..n])
  <#> factorialParam
  =#> factorialResult

factorialParam :: Parameter Lua.Exception Prelude.Integer
factorialParam =
  parameter peekIntegral "integer"
    "n"
    "number for which the factorial is computed"

factorialResult :: FunctionResults Lua.Exception Prelude.Integer
factorialResult = functionResult pushIntegral "integer" "factorial"
