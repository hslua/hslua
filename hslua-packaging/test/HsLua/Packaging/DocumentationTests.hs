{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.DocumentationTests
Copyright   : © 2021-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.DocumentationTests (tests) where

import Data.Version (makeVersion)
import HsLua.Core (top, Status (OK), Type (TypeNil, TypeString))
import HsLua.Packaging.Documentation
import HsLua.Packaging.Function
import HsLua.Marshalling (forcePeek, peekIntegral, pushIntegral, peekText)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Documentation"
  [ testGroup "Function docs"
    [ "retrieves function docs" =:
      "factorial" `shouldBeResultOf` do
        pushDocumentedFunction factorial
        Lua.setglobal (functionName factorial)
        pushDocumentedFunction documentation
        Lua.setglobal "documentation"
        OK <- Lua.dostring "return documentation(factorial)"
        TypeString <- Lua.getfield top "name"
        forcePeek $ peekText top

    , "returns nil for undocumented function" =:
      TypeNil `shouldBeResultOf` do
        pushDocumentedFunction documentation
        Lua.setglobal "documentation"
        OK <- Lua.dostring "return documentation(function () return 1 end)"
        Lua.ltype top
    ]
  ]

factorial :: DocumentedFunction Lua.Exception
factorial = defun "factorial" (liftPure $ \n -> product [1..n])
  <#> parameter (peekIntegral @Integer) "integer" "n" ""
  =#> functionResult pushIntegral "integer or string" "factorial"
  #? "Calculates the factorial of a positive integer."
  `since` makeVersion [1,0,0]
