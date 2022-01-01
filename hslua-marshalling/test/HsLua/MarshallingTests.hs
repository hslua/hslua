{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.MarshallingTests
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Test marshalling of basic values.
-}
module HsLua.MarshallingTests (tests) where

import Control.Monad ((<$!>))
import HsLua.Core
import HsLua.Marshalling.Peek
import HsLua.Marshalling.Peekers
import HsLua.Marshalling.Push
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import qualified HsLua.Marshalling.PeekTests
import qualified HsLua.Marshalling.PeekersTests
import qualified HsLua.Marshalling.PushTests
import qualified HsLua.Marshalling.UserdataTests

-- | Tests for value marshalling.
tests :: TestTree
tests = testGroup "Marshalling"
  [ HsLua.Marshalling.PeekTests.tests
  , HsLua.Marshalling.PeekersTests.tests
  , HsLua.Marshalling.PushTests.tests
  , HsLua.Marshalling.UserdataTests.tests
  , testGroup "nested"
    [ "deeply nested list" =:
      Success (mkDeeplyNested 500) `shouldBeResultOf` do
        pushNested (mkDeeplyNested 500)
        runPeek $ peekNested top
    ]
  ]

mkDeeplyNested :: Int -> Nested
mkDeeplyNested i = foldr (\_ n -> List [n]) (Element i) [1..i]

pushNested :: LuaError e => Pusher e Nested
pushNested = \case
  Element i   -> pushIntegral i
  List nested -> pushList pushNested nested

peekNested :: LuaError e => Peeker e Nested
peekNested idx = do
  liftLua (ltype idx) >>= \case
    TypeNumber  -> Element <$!> peekIntegral idx
    TypeTable   -> (List   <$!> peekList peekNested idx)
    _           -> failPeek "you dun goofed"

data Nested = Element Int | List [Nested]
  deriving (Eq, Show)
