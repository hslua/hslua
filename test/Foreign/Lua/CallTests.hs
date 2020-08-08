{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Foreign.Lua.CallTests
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for calling exposed Haskell functions.
-}
module Foreign.Lua.CallTests (tests) where

import Foreign.Lua.Core (StackIndex)
import Foreign.Lua.Call
import Foreign.Lua.Peek (peekIntegral, peekText, force)
import Foreign.Lua.Push (pushIntegral)
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
  , testGroup "documentation"
    [ "rendered docs" =:
      (T.unlines
       [ "Parameters:"
       , ""
       , "n"
       , ":   number for which the factorial is computed (integer)"
       , ""
       , "Returns:"
       , "factorial (integer)"
       ]
       @=?
       maybe "" render (functionDoc (factLuaAtIndex 0)))
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

factorialResult :: FunctionResult Integer
factorialResult = FunctionResult (pushIntegral @Integer) . Just $
  FunctionResultDoc "integer" "factorial"
