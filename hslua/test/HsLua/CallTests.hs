{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.CallTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for calling exposed Haskell functions.
-}
module HsLua.CallTests (tests) where

import Data.Maybe (fromMaybe)
import HsLua.Core (StackIndex)
import HsLua.Call
import HsLua.Peek (peekIntegral, peekRealFloat, peekText, force)
import HsLua.Push (pushIntegral, pushRealFloat)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Text as T
import qualified HsLua as Lua

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
        peekIntegral @Integer Lua.top >>= force

    , "error message" =:
      mconcat [ "retrieving function argument n"
              , "\n\texpected Integral, got 'true' (boolean)"]
      `shouldBeResultOf` do
        factLua <- factLuaAtIndex <$> Lua.gettop
        Lua.pushboolean True
        _ <- callFunction factLua
        peekText Lua.top >>= force
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
        peekIntegral @Integer Lua.top >>= force
    , "use from Lua" =:
      24
      `shouldBeResultOf` do
        pushDocumentedFunction $ factLuaAtIndex 0
        Lua.setglobal "factorial"
        Lua.loadstring "return factorial(4)" *> Lua.call 0 1
        peekIntegral @Integer Lua.top >>= force

    , "with setting an optional param" =:
      8
      `shouldBeResultOf` do
        pushDocumentedFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64)" *> Lua.call 0 1
        peekRealFloat @Double Lua.top >>= force
    , "with setting an optional param" =:
      2
      `shouldBeResultOf` do
        pushDocumentedFunction nroot
        Lua.setglobal "nroot"
        Lua.loadstring "return nroot(64, 6)" *> Lua.call 0 1
        peekRealFloat @Double Lua.top >>= force
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
        parameterDoc
          (parameter @Lua.Exception peekText "string" "test" "test param")
      )
    , "optionalParameter doc" =:
      ( ParameterDoc
        { parameterName = "test"
        , parameterDescription = "test param"
        , parameterType = "string"
        , parameterIsOptional = True
        }
        @=?
        parameterDoc
          (optionalParameter @Lua.Exception peekText "string" "test" "test param")
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

factLuaAtIndex :: StackIndex -> DocumentedFunction Lua.Exception
factLuaAtIndex idx =
  toHsFnPrecursorWithStartIndex idx (return . factorial)
  <#> factorialParam
  =#> factorialResult

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
  (FunctionResultDoc "integer" "factorial")

-- | Calculate the nth root of a number. Defaults to square root.
nroot :: DocumentedFunction Lua.Exception
nroot = toHsFnPrecursor nroot'
  <#> parameter (peekRealFloat @Double) "number" "x" ""
  <#> optionalParameter (peekIntegral @Int) "integer" "n" ""
  =#> functionResult pushRealFloat "number" "nth root"
  where
    nroot' :: Double -> Maybe Int -> Lua.LuaE e Double
    nroot' x nOpt =
      let n = fromMaybe 2 nOpt
      in return $ x ** (1 / fromIntegral n)
