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
        peekBool Lua.stackTop

    , "False" =:
      Right False `shouldBeResultOf` do
        Lua.pushboolean False
        peekBool Lua.stackTop

    , "Numbers are truthy" =:
      Right True `shouldBeResultOf` do
        Lua.pushnumber 0
        peekBool Lua.stackTop

    , "Nil is falsy" =:
      Right False `shouldBeResultOf` do
        Lua.pushnil
        peekBool Lua.stackTop

    -- no tests for failing cases, this function always succeeds.
    ]

  , testGroup "peekIntegral"
    [ "negative Int" =:
      Right (-5) `shouldBeResultOf` do
        Lua.pushinteger (-5)
        peekIntegral @Int Lua.stackTop

    , "Int as string" =:
      Right 720 `shouldBeResultOf` do
        Lua.pushstring "720"
        peekIntegral @Int Lua.stackTop

    , "fail on boolean" =:
      let msg = "expected Integral, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekIntegral @Int Lua.stackTop

    , "fail on non-numeric string" =:
      let msg = "expected Integral, got 'not a number' (string)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekIntegral @Integer Lua.stackTop
    ]

  , testGroup "peekRealFloat"
    [ "negative Float" =:
      Right (-13.37) `shouldBeResultOf` do
        Lua.pushnumber (-13.37)
        peekRealFloat @Float Lua.stackTop

    , "number as string" =:
      Right (-720.0) `shouldBeResultOf` do
        Lua.pushstring "-720"
        peekRealFloat @Float Lua.stackTop

    , "scientific notation string" =:
      Right 0.00071 `shouldBeResultOf` do
        Lua.pushstring "7.1e-4"
        peekRealFloat @Float Lua.stackTop

    , "fail on boolean" =:
      let msg = "expected RealFloat, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekRealFloat @Float Lua.stackTop

    , "fail on non-numeric string" =:
      let msg = "expected RealFloat, got 'not a number' (string)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekRealFloat @Double Lua.stackTop
    ]

  , testGroup "Strings"
    [ testGroup "peekByteString"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring bs
            peekByteString Lua.stackTop
          assert (retrieved == Right bs)

      , testProperty "retrieve integer as string" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run $ do
            Lua.pushinteger n
            peekByteString Lua.stackTop
          let numberAsByteString = Char8.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Right numberAsByteString)

      , "fails on boolean" =:
      let msg = "expected string, got 'true' (boolean)"
      in Left (errorMsg msg) `shouldBeResultOf` do
        Lua.pushboolean True
        peekByteString Lua.stackTop
      ]

    , testGroup "peekText"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring bs
            peekText Lua.stackTop
          assert (retrieved == Right (Utf8.toText bs))

      , testProperty "retrieve UTF-8 encoded Text" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring (Utf8.fromText txt)
            peekText Lua.stackTop
          assert (retrieved == Right txt)

      , testProperty "retrieve integer as Text" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run $ do
            Lua.pushinteger n
            peekText Lua.stackTop
          let numberAsByteString = T.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Right numberAsByteString)

      , "fails on nil" =:
        let msg = "expected string, got 'nil' (nil)"
        in Left (errorMsg msg) `shouldBeResultOf` do
          Lua.pushnil
          peekByteString Lua.stackTop
      ]

    , testGroup "peekString"
      [ testProperty "retrieve UTF-8 encoded string" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.pushstring (Utf8.fromString txt)
            peekString Lua.stackTop
          assert (retrieved == Right txt)

      , "fails on table" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekString Lua.stackTop

      , "fails on thread" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushthread
          peekString Lua.stackTop
      ]

    , testGroup "peekStringy"
      [ testProperty "retrieve UTF-8 encoded string as Text" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run $ do
              Lua.pushstring (Utf8.fromText txt)
              peekStringy @T.Text Lua.stackTop
            assert (retrieved == Right txt)

      , "retrieve ByteString" =:
        Right "This is an ASCII string" `shouldBeResultOf` do
          Lua.pushstring "This is an ASCII string"
          peekStringy @B.ByteString Lua.stackTop

      , "fails on table" =:
        isLeft `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekStringy @B.ByteString Lua.stackTop
      ]
    ]

  , testGroup "Containers"
    [ testGroup "peekList"
      [ "empty list" =:
        Right [] `shouldBeResultOf` do
          Lua.newtable
          peekList peekBool Lua.stackTop

      , testProperty "list of strings" $ \lst -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.newtable
            zipWithM_
              (\i s -> Lua.pushstring s *>
                       Lua.rawseti (Lua.nthFromTop 2) i)
              [1..]
              lst
            peekList peekByteString Lua.stackTop
          assert (retrieved == Right lst)

      , "string keys are not in list" =:
        Right [] `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = 'world'}"
          peekList peekByteString Lua.stackTop

      , "missing pair causes an error" =:
        isLeft `shouldHoldForResultOf` do
          pushLuaExpr "{[1] = 'hello', [2] = 'world', [4] = 'nope'}"
          peekList peekByteString Lua.stackTop
      ]

    , testGroup "peekSet"
      [ "empty set" =:
        Right Set.empty `shouldBeResultOf` do
          Lua.newtable
          peekSet peekBool Lua.stackTop

      , testProperty "set of strings" $ \set -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.newtable
            forM_ (Set.toList set) $ \x -> do
              Lua.pushstring x
              Lua.pushboolean True
              Lua.rawset (Lua.nthFromTop 3)
            peekSet peekByteString Lua.stackTop
          assert (retrieved == Right set)

      , "keys with falsy values are not in set" =:
        Right (Set.fromList [1,3]) `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = false, [3] = 5}"
          peekSet (peekIntegral @Int) Lua.stackTop

      , "fails if element peeker fails" =:
        let errorStack = [ "retrieving Set"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         , "expected string, got 'true' (boolean)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true, [true] = false }"
          peekSet peekText Lua.stackTop
      ]

    , testGroup "peekMap"
      [ "empty map" =:
        Right Map.empty `shouldBeResultOf` do
          Lua.newtable
          peekMap peekText peekText Lua.stackTop

      , "tables become maps" =:
        Right (Map.fromList [("one", 1), ("two", 2)]) `shouldBeResultOf` do
          pushLuaExpr "{ one = 1, two = 2}"
          peekMap peekText (peekIntegral @Int) Lua.stackTop

      , "fails if key peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         , "expected Integral, got 'NaN' (string)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true }"
          peekMap (peekIntegral @Int) peekBool Lua.stackTop

      , "fails if value peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving value"
                         , "expected string, got 'true' (boolean)"]
        in Left (PeekError $ NonEmpty.fromList errorStack) `shouldBeResultOf` do
          pushLuaExpr "{ [42] = true }"
          peekMap (peekIntegral @Int) peekText Lua.stackTop
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
