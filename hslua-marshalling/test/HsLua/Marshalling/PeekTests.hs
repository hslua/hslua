{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Marshalling.PeekTests
Copyright   : © 2020-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for Haskell-value retriever functions.
-}
module HsLua.Marshalling.PeekTests (tests) where

import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (fromMaybe)
import HsLua.Marshalling.Peek
import Test.Tasty.HsLua ((=:), pushLuaExpr, shouldBeResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Peek"
  [ testGroup "helper"
    [ "retrieving" =:
      Failure @() "message" ["retrieving context"] `shouldBeResultOf`
      runPeek (retrieving "context" $ failPeek "message")

    , "withContext" =:
      Failure @() "message" ["context"] `shouldBeResultOf`
      runPeek (withContext "context" $ failPeek "message")

    , let firstindex idx = do
            Lua.rawgeti idx 1
            fromMaybe 0 <$> Lua.tointeger Lua.top <* Lua.pop 1
      in testGroup "toPeeker"
      [ "passes result through" =:
        Success 1337 `shouldBeResultOf` do
          pushLuaExpr "{1337}"
          runPeeker (toPeeker firstindex) Lua.top

      , "catches error" =:
        let msg = "Lua exception: table expected, got number"
        in
          Failure msg [] `shouldBeResultOf` do
          Lua.pushinteger 1337
          runPeeker (toPeeker firstindex) Lua.top
      ]
    ]

  , testGroup "Peek"
    [ "lazy alternative" =:
      Success @Int 5 `shouldBeResultOf` runPeek
        (return 5 <|> error "nope")
    ]
  ]
