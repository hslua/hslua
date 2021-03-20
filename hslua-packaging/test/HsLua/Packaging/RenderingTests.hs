{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.RenderingTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.RenderingTests (tests) where

import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
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
  let factorialDocs = T.unlines
        [ "Calculates the factorial of a positive integer."
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
        , " - factorial (integer)"
        ]
      nrootDocs = T.intercalate "\n"
        [ "### nroot (x, n)"
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
        , " - nth root (number)"
        ]
  in
    [ testGroup "Function"
      [ testCase "rendered docs" $
        factorialDocs @=?
        renderFunctionDoc (functionDoc factorial)
      ]
    , testGroup "Module"
      [ testCase "module docs"
        (T.unlines
         [ "# mymath"
         , ""
         , "A math module."
         , ""
         , "## Functions"
         , ""
         , "### factorial (n)"
         , ""
         , factorialDocs
         , nrootDocs
         ] @=?
         render mymath)
      ]
    ]

-- | Calculate the nth root of a number. Defaults to square root.
nroot :: DocumentedFunction Lua.Exception
nroot = defun "nroot" $ toHsFnPrecursor nroot'
  <#> parameter (peekRealFloat @Double) "number" "x" ""
  <#> optionalParameter (peekIntegral @Int) "integer" "n" ""
  =#> functionResult pushRealFloat "number" "nth root"
  where
    nroot' :: Double -> Maybe Int -> Lua.LuaE e Double
    nroot' x nOpt =
      let n = fromMaybe 2 nOpt
      in return $ x ** (1 / fromIntegral n)

mymath :: Module Lua.Exception
mymath = Module
  { moduleName = "mymath"
  , moduleDescription = "A math module."
  , moduleFields = []
  , moduleFunctions = [ factorial, nroot ]
  }

-- | Calculate the factorial of a number.
factorial :: DocumentedFunction Lua.Exception
factorial = defun "factorial"
   $  toHsFnPrecursor (\n -> return $ product [1..n])
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
