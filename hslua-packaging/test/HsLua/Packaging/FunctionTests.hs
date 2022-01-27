{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.FunctionTests
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.FunctionTests (tests) where

import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua.Core (StackIndex, top)
import HsLua.Packaging.Convenience
import HsLua.Packaging.Documentation (getdocumentation)
import HsLua.Packaging.Function
import HsLua.Packaging.Types
import HsLua.Marshalling
  ( forcePeek, peekIntegral, peekRealFloat, peekText
  , pushIntegral, pushRealFloat)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified HsLua.Core as Lua

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Call"
  [ testGroup "push Haskell function"
    [ "DocumentedFunction building" =:
      720
      `shouldBeResultOf` do
        factLua <- factLuaAtIndex <$> Lua.gettop
        Lua.pushinteger 6
        _ <- callFunction factLua
        forcePeek $ peekIntegral @Integer Lua.top

    , "error message" =:
      mconcat [ "Integral expected, got boolean\n"
              , "\twhile retrieving function argument n\n"
              , "\twhile retrieving arguments for function factorial"]
      `shouldBeResultOf` do
        factLua <- factLuaAtIndex <$> Lua.gettop
        Lua.pushboolean True
        _ <- callFunction factLua
        forcePeek $ peekText Lua.top
    ]
  , testGroup "use as C function"
    [ "push factorial" =:
      Lua.TypeFunction
      `shouldBeResultOf` do
        pushDocumentedFunction $ factLuaAtIndex 0
        Lua.ltype Lua.top
    , "call factorial" =:
      120
      `shouldBeResultOf` do
        pushDocumentedFunction $ factLuaAtIndex 0
        Lua.pushinteger 5
        Lua.call 1 1
        forcePeek $ peekIntegral @Integer Lua.top
    , "use from Lua" =:
      24
      `shouldBeResultOf` do
        pushDocumentedFunction $ factLuaAtIndex 0
        Lua.setglobal "factorial"
        Lua.loadstring "return factorial(4)" *> Lua.call 0 1
        forcePeek $ peekIntegral @Integer Lua.top

    , "with setting an optional param" =:
      8
      `shouldBeResultOf` do
        pushDocumentedFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64)" *> Lua.call 0 1
        forcePeek $ peekRealFloat @Double Lua.top
    , "with setting an optional param" =:
      2
      `shouldBeResultOf` do
        pushDocumentedFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64, 6)" *> Lua.call 0 1
        forcePeek $ peekRealFloat @Double Lua.top
    ]

  , testGroup "documentation access"
    [ "pushDocumentedFunction pushes one value" =:
      1 `shouldBeResultOf` do
        oldtop <- Lua.gettop
        pushDocumentedFunction (factLuaAtIndex 0)
        newtop <- Lua.gettop
        pure (newtop - oldtop)

    , "getdocumentation" =:
      "factorial" `shouldBeResultOf` do
        pushDocumentedFunction (factLuaAtIndex 0)
        Lua.TypeTable <- getdocumentation top
        Lua.TypeString <- Lua.getfield top "name"
        forcePeek (peekText top)

    , "undocumented value" =:
      Lua.TypeNil `shouldBeResultOf` do
        Lua.pushboolean True
        getdocumentation top
    ]

  , testGroup "helpers"
    [ "parameter doc" =:
      ( ParameterDoc
        { parameterName = "test"
        , parameterDescription = "test param"
        , parameterType = "string"
        , parameterIsOptional = False
        }
        @=?
        parameterDoc
          (parameter @Lua.Exception peekText "string" "test" "test param")
      )
    , "optional parameter doc" =:
      ( ParameterDoc
        { parameterName = "test"
        , parameterDescription = "test param"
        , parameterType = "string"
        , parameterIsOptional = True
        }
        @=?
        parameterDoc
          (opt (textParam @Lua.Exception "test" "test param"))
      )
    , "functionResult doc" =:
      ( [ ResultValueDoc
          { resultValueDescription = "int result"
          , resultValueType = "integer"
          }
        ]
        @=?
        fnResultDoc <$>
          functionResult (pushIntegral @Int) "integer" "int result"
      )
    ]
  ]

factLuaAtIndex :: StackIndex -> DocumentedFunction Lua.Exception
factLuaAtIndex idx =
  toHsFnPrecursor idx "factorial" (liftPure factorial)
  <#> factorialParam
  =#> factorialResult
  #? "Calculates the factorial of a positive integer."
  `since` makeVersion [1,0,0]

-- | Calculate the factorial of a number.
factorial :: Integer -> Integer
factorial n = product [1..n]

factorialParam :: Parameter Lua.Exception Integer
factorialParam = Parameter
  { parameterDoc = ParameterDoc
    { parameterName = "n"
    , parameterType = "integer"
    , parameterDescription = "number for which the factorial is computed"
    , parameterIsOptional = False
    }
  , parameterPeeker = peekIntegral @Integer
  }

factorialResult :: FunctionResults Lua.Exception Integer
factorialResult = (:[]) $ FunctionResult
  (pushIntegral @Integer)
  (ResultValueDoc "integer" "factorial")

-- | Calculate the nth root of a number. Defaults to square root.
nroot :: DocumentedFunction Lua.Exception
nroot = defun "nroot"
  ### liftPure2 nroot'
  <#> parameter (peekRealFloat @Double) "number" "x" ""
  <#> opt (integralParam @Int "n" "")
  =#> functionResult pushRealFloat "number" "nth root"
  where
    nroot' :: Double -> Maybe Int -> Double
    nroot' x nOpt =
      let n = fromMaybe 2 nOpt
      in x ** (1 / fromIntegral n)
