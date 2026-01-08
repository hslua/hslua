{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.ModuleTests
Copyright   : © 2019-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests creating and loading of modules with Haskell.
-}
module HsLua.Packaging.ModuleTests (tests) where

import HsLua.Core
import HsLua.Marshalling
  ( forcePeek, peekIntegral, peekString, pushIntegral, pushText )
import HsLua.Packaging.Documentation
import HsLua.Packaging.Function
import HsLua.Packaging.Module
import HsLua.Packaging.UDType (deftype)
import HsLua.Packaging.Types
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Module"
  [ testGroup "creation helpers"
    [ "pushing a module produces a table" =:
      Lua.TypeTable `shouldBeResultOf` do
        pushModule $ defmodule "test"
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
        TypeUserdata <- getdocumentation top
        forcePeek $ moduleDocName <$> peekModuleDoc Lua.top

    , "function name in docs is prefixed with module name" =:
      "mymath.factorial" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeUserdata <- getdocumentation top
        mdldoc <- forcePeek $ peekModuleDoc Lua.top
        case moduleDocFunctions mdldoc of
          fd:_ -> pure $ funDocName fd
          _    -> fail "No documented functions"

    , "function doc is shared" =:
      True `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        pushvalue top
        setglobal "mymath"
        -- get doc table via module docs
        TypeUserdata <- getdocumentation top
        fndoc <- forcePeek $
          moduleDocFunctions <$> peekModuleDoc Lua.top >>= \case
            fd:_ -> pure fd
            _    -> fail "No documented functions"

        -- get the function documenation via Lua
        OK <- dostring "return mymath.factorial"
        TypeUserdata <- getdocumentation top
        fndoc' <- forcePeek $ peekFunctionDoc Lua.top
        -- must be the same
        return (fndoc == fndoc')

    , "first field name in docs" =:
      "mymath.unit" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeUserdata <- getdocumentation top
        mdl <- forcePeek $ peekModuleDoc Lua.top
        case moduleDocFields mdl of
          f:_ -> pure $ fieldDocName f
          [] -> fail "No fields"

    , "document object has associated types" =:
      ["Void"] `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        TypeUserdata <- getdocumentation top
        mdl <- forcePeek $ peekModuleDoc Lua.top
        return . map typeDocName $ moduleDocTypes mdl
    ]
  ]

mymath :: Module Lua.Exception
mymath = defmodule "mymath"
  `withFields`
    [ deffield "unit"
      `withType` "integer"
      `withDescription` "additive unit"
      `withValue` pushinteger 1
    ]
  `withFunctions` [factorial]
  `withOperations`
    [ (,) Call $ lambda
      ### (1 <$ pushText "call me maybe")
      =?> "call result"
    ]
  `associateType` deftype "Void" [] []

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
