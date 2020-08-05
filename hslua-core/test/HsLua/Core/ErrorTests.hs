{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-| Tests for error handling.
-}
module HsLua.Core.ErrorTests (tests) where

import Control.Applicative ((<|>), empty)
import Data.Either (isLeft)
import HsLua.Core (Lua)
import Test.Tasty.HsLua ( (=:), (?:), shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Error"
  [ "try catches errors" =:
    isLeft `shouldHoldForResultOf` Lua.try
      (Lua.throwException "test" :: Lua ())

  , "second alternative is used when first fails" ?:
    (Lua.throwException @Lua.Exception "test" <|> return True)

  , "Applicative.empty implementation throws an exception" =:
    isLeft `shouldHoldForResultOf` Lua.try (empty :: Lua ())
  ]
