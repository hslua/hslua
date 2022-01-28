{-# LANGUAGE OverloadedStrings #-}
{-| Tests for the primary library.
-}
module HsLua.Core.PrimaryTests (tests) where

import HsLua.Core
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Primary"
  [ testGroup "rotate'"
    [ "rotates stack" =:
    (Just 2, Just 1, Just 4, Just 3) `shouldBeResultOf` do
        pushnumber 4
        pushnumber 3
        pushnumber 2
        pushnumber 1
        rotate (nth 4) 2
        (,,,)
          <$> tonumber (nth 4)
          <*> tonumber (nth 3)
          <*> tonumber (nth 2)
          <*> tonumber (nth 1)
    ]
  ]
