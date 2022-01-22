{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.ModuleTests
Copyright   : © 2019-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests creating and loading of modules with Haskell.
-}
module HsLua.Packaging.ModuleTests (tests) where

import HsLua.Marshalling
  (Result (Success), peekIntegral, peekString, pushIntegral, pushText, runPeek)
import HsLua.Packaging.Function
import HsLua.Packaging.Module
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
      Success 24 `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        _ <- Lua.dostring $ mconcat
             [ "local mymath = require 'mymath'\n"
             , "return mymath.factorial(4)"
             ]
        runPeek $ peekIntegral @Integer Lua.top

    , "call module as function" =:
      Success "call me maybe" `shouldBeResultOf` do
        Lua.openlibs
        registerModule mymath
        _ <- Lua.dostring "return (require 'mymath')()"
        runPeek $ peekString Lua.top

    ]
  ]

mymath :: Module Lua.Exception
mymath = Module
  { moduleName = "mymath"
  , moduleDescription = "A math module."
  , moduleFields = []
  , moduleFunctions = [factorial]
  , moduleOperations =
    [ (,) Call $ lambda
      ### (1 <$ pushText "call me maybe")
      =?> "call result"
    ]
  }

factorial :: DocumentedFunction Lua.Exception
factorial =
  defun "factorial"
  ### liftPure (\n -> product [1..n])
  <#> factorialParam
  =#> factorialResult

factorialParam :: Parameter Lua.Exception Integer
factorialParam =
  parameter (peekIntegral @Integer) "integer"
    "n"
    "number for which the factorial is computed"

factorialResult :: FunctionResults Lua.Exception Integer
factorialResult =
  functionResult (pushIntegral @Integer) "integer" "factorial"
