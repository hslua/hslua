{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
Module      : HsLua.Packaging.RenderingTests
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.RenderingTests (tests) where

import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua.Packaging.Convenience
import HsLua.Packaging.Function
import HsLua.Packaging.Module
import HsLua.Packaging.Rendering
import HsLua.Marshalling
  (peekIntegral, peekRealFloat, pushIntegral, pushRealFloat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import qualified Data.Text as T
import qualified HsLua.Core as Lua

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Rendering" $
  let factorialDocs = T.intercalate "\n"
        [ "factorial (n)"
        , ""
        , "Calculates the factorial of a positive integer."
        , ""
        , "*Since: 1.0.0*"
        , ""
        , "Parameters:"
        , ""
        , "n"
        , ":   number for which the factorial is computed (integer)"
        , ""
        , "Returns:"
        , ""
        , " -  factorial (integer)"
        ]
      nrootDocs = T.intercalate "\n"
        [ "nroot (x, n)"
        , ""
        , "Parameters:"
        , ""
        , "x"
        , ":    (number)"
        , ""
        , "n"
        , ":    (integer)"
        , ""
        , "Returns:"
        , ""
        , " -  nth root (number)"
        ]
      eulerDocs = T.intercalate "\n"
        [ "euler_mascheroni"
        , ""
        , "Euler-Mascheroni constant"
        ]
  in
    [ testGroup "Function"
      [ testCase "rendered docs" $
        factorialDocs @=?
        renderFunction factorial
      ]
    , testGroup "Module"
      [ testCase "module docs"
        (T.unlines
         [ "# mymath"
         , ""
         , "A math module."
         , ""
         , "### " `T.append` eulerDocs
         , ""
         , "## Functions"
         , ""
         , "### " `T.append` factorialDocs
         , ""
         , "### " `T.append` nrootDocs
         ] @=?
         render mymath)
      ]
    ]

-- | Calculate the nth root of a number. Defaults to square root.
nroot :: DocumentedFunction Lua.Exception
nroot = defun "nroot" (liftPure2 nroot')
  <#> parameter (peekRealFloat @Double) "number" "x" ""
  <#> opt (integralParam @Int "n" "")
  =#> functionResult pushRealFloat "number" "nth root"
  where
    nroot' :: Double -> Maybe Int -> Double
    nroot' x nOpt =
      let n = fromMaybe 2 nOpt
      in x ** (1 / fromIntegral n)

mymath :: Module Lua.Exception
mymath = Module
  { moduleName = "mymath"
  , moduleDescription = "A math module."
  , moduleFields = [euler_mascheroni]
  , moduleFunctions = [ factorial, nroot ]
  , moduleOperations = []
  }

-- | Euler-Mascheroni constant
euler_mascheroni :: Field Lua.Exception
euler_mascheroni = Field
  { fieldName = "euler_mascheroni"
  , fieldDescription = "Euler-Mascheroni constant"
  , fieldPushValue = pushRealFloat @Double
                     0.57721566490153286060651209008240243
  }

-- | Calculate the factorial of a number.
factorial :: DocumentedFunction Lua.Exception
factorial = defun "factorial"
  ### liftPure (\n -> product [1..n])
  <#> factorialParam
  =#> factorialResult
  #? "Calculates the factorial of a positive integer."
  `since` makeVersion [1,0,0]

factorialParam :: Parameter Lua.Exception Integer
factorialParam =
  parameter peekIntegral "integer"
    "n"
    "number for which the factorial is computed"

factorialResult :: FunctionResults Lua.Exception Integer
factorialResult =
  functionResult pushIntegral "integer" "factorial"
