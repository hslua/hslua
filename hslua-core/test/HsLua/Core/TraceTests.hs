{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Core.TraceTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Test call functions that produce traces on error.
-}
module HsLua.Core.TraceTests (tests) where

import Data.ByteString (isInfixOf)
import HsLua.Core as Lua
import Test.Tasty.HsLua ( (=:), shouldBeResultOf, shouldHoldForResultOf
                        , pushLuaExpr)
import Test.Tasty (TestTree, testGroup)
import qualified Data.List as List

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Trace"
  [ testGroup "pcallTrace"
    [ "Calls the function" =:
      "motor" `shouldBeResultOf` do
        openlibs
        pushLuaExpr "function () return 'motor' end"
        OK <- pcallTrace 0 1
        tostring' top

    , "Adds a traceback" =:
      ("\nstack traceback:\n" `isInfixOf`) `shouldHoldForResultOf` do
        openlibs
        pushLuaExpr "function (b) error(tostring(b)) end"
        pushinteger 23
        ErrRun <- pcallTrace 1 1
        tostring' top
    ]
  , testGroup "callTrace"
    [ "Calls the function" =:
      "motor" `shouldBeResultOf` do
        openlibs
        pushLuaExpr "function () return 'motor' end"
        callTrace 0 1
        tostring' top

    , "Adds a traceback" =:
      ("\nstack traceback:\n" `List.isInfixOf`) `shouldHoldForResultOf` do
        either show (const $ Prelude.error "should not succeed") <$> try
          (do openlibs
              pushLuaExpr "function (b) error(tostring(b)) end"
              pushinteger 23
              callTrace 1 1
              tostring' top)
    ]
  ]
