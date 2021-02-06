{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Foreign.Lua.CallTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for calling exposed Haskell functions.
-}
module Foreign.Lua.CallTests (tests) where

import Data.Maybe (fromMaybe)
import Foreign.Lua.Core (StackIndex)
import Foreign.Lua.Call
import Foreign.Lua.Peek (peekIntegral, peekRealFloat, peekText, force)
import Foreign.Lua.Push (pushIntegral, pushRealFloat)
import Test.HsLua.Util ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Text as T
import qualified Foreign.Lua as Lua

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Call"
  [ testGroup "push Haskell function"
    [ "HaskellFunction building" =:
      720
      `shouldBeResultOf` do
        factLua <- factLuaAtIndex <$> Lua.gettop
        Lua.pushinteger 6
        _ <- callFunction factLua
        peekIntegral @Integer Lua.stackTop >>= force

    , "error message" =:
      mconcat [ "retrieving function argument n"
              , "\n\texpected Integral, got 'true' (boolean)"]
      `shouldBeResultOf` do
        factLua <- factLuaAtIndex <$> Lua.gettop
        Lua.pushboolean True
        _ <- callFunction factLua
        peekText Lua.stackTop >>= force
    ]
  , testGroup "use as C function"
    [ "push factorial" =:
      Lua.TypeFunction
      `shouldBeResultOf` do
        pushHaskellFunction $ factLuaAtIndex 0
        Lua.ltype Lua.stackTop
    , "call factorial" =:
      120
      `shouldBeResultOf` do
        pushHaskellFunction $ factLuaAtIndex 0
        Lua.pushinteger 5
        Lua.call 1 1
        peekIntegral @Integer Lua.stackTop >>= force
    , "use from Lua" =:
      24
      `shouldBeResultOf` do
        pushHaskellFunction $ factLuaAtIndex 0
        Lua.setglobal "factorial"
        Lua.loadstring "return factorial(4)" *> Lua.call 0 1
        peekIntegral @Integer Lua.stackTop >>= force

    , "with setting an optional param" =:
      8
      `shouldBeResultOf` do
        pushHaskellFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64)" *> Lua.call 0 1
        peekRealFloat @Double Lua.stackTop >>= force
    , "with setting an optional param" =:
      2
      `shouldBeResultOf` do
        pushHaskellFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64, 6)" *> Lua.call 0 1
        peekRealFloat @Double Lua.stackTop >>= force
    ]
  , testGroup "documentation"
    [ "rendered docs" =:
      (T.unlines
       [ "Parameters:"
       , ""
       , "n"
       , ":   number for which the factorial is computed (integer)"
       , ""
       , "Returns:"
       , ""
       , " - factorial (integer)"
       ]
       @=?
       maybe "" render (functionDoc (factLuaAtIndex 0)))
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
        parameterDoc (parameter peekText "string" "test" "test param")
      )
    , "optionalParameter doc" =:
      ( ParameterDoc
        { parameterName = "test"
        , parameterDescription = "test param"
        , parameterType = "string"
        , parameterIsOptional = True
        }
        @=?
        parameterDoc (optionalParameter peekText "string" "test" "test param")
      )
    , "functionResult doc" =:
      ( FunctionResultDoc
        { functionResultDescription = "int result"
        , functionResultType = "integer"
        }
        @=?
        (fnResultDoc . head $
         functionResult (pushIntegral @Int) "integer" "int result")
      )
    ]
  ]

factLuaAtIndex :: StackIndex -> HaskellFunction
factLuaAtIndex idx =
  toHsFnPrecursorWithStartIndex idx factorial
  <#> factorialParam
  =#> factorialResult

-- | Calculate the factorial of a number.
factorial :: Integer -> Integer
factorial n = product [1..n]

factorialParam :: Parameter Integer
factorialParam = Parameter
  { parameterDoc = ParameterDoc
    { parameterName = "n"
    , parameterType = "integer"
    , parameterDescription = "number for which the factorial is computed"
    , parameterIsOptional = False
    }
  , parameterPeeker = peekIntegral @Integer
  }

factorialResult :: FunctionResults Integer
factorialResult = (:[]) $ FunctionResult
  (pushIntegral @Integer)
  (FunctionResultDoc "integer" "factorial")

-- | Calculate the nth root of a number. Defaults to square root.
nroot :: HaskellFunction
nroot = toHsFnPrecursor nroot'
  <#> parameter (peekRealFloat @Double) "number" "x" ""
  <#> optionalParameter (peekIntegral @Int) "integer" "n" ""
  =#> functionResult pushRealFloat "number" "nth root"
  where
    nroot' :: Double -> Maybe Int -> Double
    nroot' x nOpt =
      let n = fromMaybe 2 nOpt
      in x ** (1 / fromIntegral n)
