{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.PeekTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for Haskell-value retriever functions.
-}
module HsLua.PeekTests (tests) where

import Control.Monad (forM_, zipWithM_)
import Data.Either (isLeft)
import HsLua.Peek

import Test.HsLua.Arbitrary ()
import Test.HsLua.Util ( (=:), pushLuaExpr, shouldBeResultOf
                       , shouldHoldForResultOf)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified HsLua.Utf8 as Utf8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Peek"
  [ testGroup "peekBool"
    [ "True" =:
      Right True `shouldBeResultOf` do
        Lua.pushboolean True
        peekBool Lua.top

    , "False" =:
      Right False `shouldBeResultOf` do
        Lua.pushboolean False
        peekBool Lua.top

    , "Numbers are truthy" =:
      Right True `shouldBeResultOf` do
        Lua.pushnumber 0
        peekBool Lua.top

    , "Nil is falsy" =:
      Right False `shouldBeResultOf` do
        Lua.pushnil
        peekBool Lua.top

    -- no tests for failing cases, this function always succeeds.
    ]

  , testGroup "peekIntegral"
    [ "negative Int" =:
      Right (-5) `shouldBeResultOf` do
        Lua.pushinteger (-5)
        peekIntegral @Int Lua.top

    , "Int as string" =:
      Right 720 `shouldBeResultOf` do
        Lua.pushstring "720"
        peekIntegral @Int Lua.top

    , "fail on boolean" =:
      let msg = "expected Integral, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekIntegral @Int Lua.top

    , "fail on non-numeric string" =:
      let msg = "expected Integral, got 'not a number' (string)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekIntegral @Integer Lua.top
    ]

  , testGroup "peekRealFloat"
    [ "negative Float" =:
      Right (-13.37) `shouldBeResultOf` do
        Lua.pushnumber (-13.37)
        peekRealFloat @Float Lua.top

    , "number as string" =:
      Right (-720.0) `shouldBeResultOf` do
        Lua.pushstring "-720"
        peekRealFloat @Float Lua.top

    , "scientific notation string" =:
      Right 0.00071 `shouldBeResultOf` do
        Lua.pushstring "7.1e-4"
        peekRealFloat @Float Lua.top

    , "fail on boolean" =:
      let msg = "expected RealFloat, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekRealFloat @Float Lua.top

    , "fail on non-numeric string" =:
      let msg = "expected RealFloat, got 'not a number' (string)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekRealFloat @Double Lua.top
    ]

  , testGroup "Strings"
    [ testGroup "peekByteString"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring bs
            peekByteString Lua.top
          assert (retrieved == Right bs)

      , testProperty "retrieve integer as string" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run $ do
            Lua.pushinteger n
            peekByteString Lua.top
          let numberAsByteString = Char8.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Right numberAsByteString)

      , "fails on boolean" =:
      let msg = "expected string, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekByteString Lua.top
      ]

    , testGroup "peekText"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring bs
            peekText Lua.top
          assert (retrieved == Right (Utf8.toText bs))

      , testProperty "retrieve UTF-8 encoded Text" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring (Utf8.fromText txt)
            peekText Lua.top
          assert (retrieved == Right txt)

      , testProperty "retrieve integer as Text" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run $ do
            Lua.pushinteger n
            peekText Lua.top
          let numberAsByteString = T.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Right numberAsByteString)

      , "fails on nil" =:
        let msg = "expected string, got 'nil' (nil)"
        in Left (errorMsg msg) `shouldBeResultOf` do
          Lua.pushnil
          peekByteString Lua.top
      ]

    , testGroup "peekString"
      [ testProperty "retrieve UTF-8 encoded string" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring (Utf8.fromString txt)
            peekString Lua.top
          assert (retrieved == Right txt)

      , "fails on table" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekString Lua.top

      , "fails on thread" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushthread
          peekString Lua.top
      ]

    , testGroup "peekStringy"
      [ testProperty "retrieve UTF-8 encoded string as Text" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run $ do
              Lua.pushstring (Utf8.fromText txt)
              peekStringy @T.Text Lua.top
            assert (retrieved == Right txt)

      , "retrieve ByteString" =:
        Right "This is an ASCII string" `shouldBeResultOf` do
          Lua.pushstring "This is an ASCII string"
          peekStringy @B.ByteString Lua.top

      , "fails on table" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekStringy @B.ByteString Lua.top
      ]
    ]

  , testGroup "Containers"
    [ testGroup "peekList"
      [ "empty list" =:
        Right [] `shouldBeResultOf` do
          Lua.newtable
          peekList peekBool Lua.top

      , testProperty "list of strings" $ \lst -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.newtable
            zipWithM_
              (\i s -> Lua.pushstring s *>
                       Lua.rawseti (Lua.nth 2) i)
              [1..]
              lst
            peekList peekByteString Lua.top
          assert (retrieved == Right lst)

      , "string keys are not in list" =:
        Right [] `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = 'world'}"
          peekList peekByteString Lua.top

      , "missing pair causes an error" =:
        isLeft `shouldHoldForResultOf` do
          pushLuaExpr "{[1] = 'hello', [2] = 'world', [4] = 'nope'}"
          peekList peekByteString Lua.top
      ]

    , testGroup "peekSet"
      [ "empty set" =:
        Right Set.empty `shouldBeResultOf` do
          Lua.newtable
          peekSet peekBool Lua.top

      , testProperty "set of strings" $ \set -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.newtable
            forM_ (Set.toList set) $ \x -> do
              Lua.pushstring x
              Lua.pushboolean True
              Lua.rawset (Lua.nth 3)
            peekSet peekByteString Lua.top
          assert (retrieved == Right set)

      , "keys with falsy values are not in set" =:
        Right (Set.fromList [1,3]) `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = false, [3] = 5}"
          peekSet (peekIntegral @Int) Lua.top

      , "fails if element peeker fails" =:
        let errorStack = [ "retrieving Set"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         , "expected string, got 'true' (boolean)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true, [true] = false }"
          peekSet peekText Lua.top
      ]

    , testGroup "peekMap"
      [ "empty map" =:
        Right Map.empty `shouldBeResultOf` do
          Lua.newtable
          peekMap peekText peekText Lua.top

      , "tables become maps" =:
        Right (Map.fromList [("one", 1), ("two", 2)]) `shouldBeResultOf` do
          pushLuaExpr "{ one = 1, two = 2}"
          peekMap peekText (peekIntegral @Int) Lua.top

      , "fails if key peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         , "expected Integral, got 'NaN' (string)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true }"
          peekMap (peekIntegral @Int) peekBool Lua.top

      , "fails if value peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving value"
                         , "expected string, got 'true' (boolean)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ [42] = true }"
          peekMap (peekIntegral @Int) peekText Lua.top
      ]
    ]

  , testGroup "combinators"
    [ "optional with nil" =:
      Right Nothing `shouldBeResultOf` do
        Lua.pushnil
        optional peekString Lua.top
    , "optional with number" =:
      Right (Just 23) `shouldBeResultOf` do
        Lua.pushinteger 23
        optional (peekIntegral @Int) Lua.top
    ]
  ]
