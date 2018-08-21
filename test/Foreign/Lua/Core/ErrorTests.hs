{-# LANGUAGE OverloadedStrings #-}
{-| Tests for error handling.
-}
module Foreign.Lua.Core.ErrorTests (tests) where

import Control.Applicative ((<|>), empty)
import Data.Either (isLeft)
import Foreign.Lua (Lua)
import Test.HsLua.Util ( (=:), shouldBeResultOf, shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Error"
  [ "try catches errors" =:
    isLeft `shouldHoldForResultOf` Lua.try (Lua.throwException "test" :: Lua ())

  , "second alternative is used when first fails" =:
    True `shouldBeResultOf` (Lua.throwException "test" <|> return True)

  , "Applicative.empty implementation throws an exception" =:
    isLeft `shouldHoldForResultOf` Lua.try (empty :: Lua ())
  ]
