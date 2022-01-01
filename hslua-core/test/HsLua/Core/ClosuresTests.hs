{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  HsLua.Core.ClosuresTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Test exposing Haskell functions to Lua.
-}
module HsLua.Core.ClosuresTests (tests) where

import Control.Monad (forM_, void)
import Data.Maybe (fromMaybe)
import HsLua.Core as Lua
import Test.Tasty.HsLua ((=:), (?:), shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Closures"
  [ "Haskell functions are callable from Lua" =:
    Just (113 :: Lua.Integer) `shouldBeResultOf` do
      -- add 23
      pushHaskellFunction $ do
        i <- tointeger (nthBottom 1)
        pushinteger (fromMaybe 0 i + 42)
        return (NumResults 1)
      pushinteger 71
      call 1 1
      tointeger top

  , "Haskell functions have the Lua type C function" ?: do
      pushHaskellFunction (return 0 :: Lua NumResults)
      iscfunction top

  -- The following test case will hang if there are issues with the way
  -- functions are garbage collection.
  , "function garbage collection" =:
    () `shouldBeResultOf` do
      let pushAndPopAdder n = do
            let fn :: Lua NumResults
                fn = do
                  x <- fromMaybe 0 <$> tointeger (nthBottom 1)
                  pushinteger (x + n)
                  return (NumResults 1)
            pushHaskellFunction fn
            pop 1
      forM_ [1..5000::Lua.Integer] pushAndPopAdder
      void $ gc Lua.GCCollect
  ]
