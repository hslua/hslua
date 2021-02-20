{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.PushTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Test pushing Haskell values to the stack.
-}
module HsLua.PushTests (tests) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import HsLua.Core (Lua, Number)
import HsLua.Push

import Test.HsLua.Arbitrary ()
import Test.HsLua.Util ((=:), pushLuaExpr)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified HsLua as Lua
import qualified HsLua.Utf8 as Utf8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Push"
  [ testGroup "pushBool"
    [ "True" =:
      assertLuaEqual (pushBool True) "true"
    , "False" =:
      assertLuaEqual (pushBool False) "false"

    , testSingleElementProperty pushBool
    ]

  , testGroup "pushIntegral"
    [ testGroup "@Int"
      [ "0" =:
        assertLuaEqual (pushIntegral @Int 0) "0"
      , "23" =:
        assertLuaEqual (pushIntegral @Int 23) "23"
      , "-5" =:
        assertLuaEqual (pushIntegral @Int (-5)) "-5"
      , testSingleElementProperty (pushIntegral @Int)
      ]
    , testGroup "@Integer"
      [ "2^128 + 1" =:
        assertLuaEqual (pushIntegral @Integer
                         340282366920938463463374607431768211457)
                       "'340282366920938463463374607431768211457'"
      , "-2^129 + 1" =:
        assertLuaEqual (pushIntegral @Integer
                        (-680564733841876926926749214863536422911))
                       "'-680564733841876926926749214863536422911'"
      , testSingleElementProperty (pushIntegral @Integer)
      ]
    ]

  , testGroup "pushRealFloat"
    [ testGroup "@Number"
      [ "0.0" =:
        assertLuaEqual (pushRealFloat @Number 0.0) "0.0"
      , "42.0" =:
        assertLuaEqual (pushRealFloat @Number 42.0) "42.0"
      , "0.1" =:
        assertLuaEqual (pushRealFloat @Number 0.1) "0.1"
      , "-13.37" =:
        assertLuaEqual (pushRealFloat @Number (-13.37)) "-13.37"
      , testSingleElementProperty (pushRealFloat @Number)
      ]

    -- This test may fail if Lua is compiled with Float as the Number
    -- type. Usually though, numbers are doubles.
    , testGroup "@Float pushes strings"
      [ "0.0" =:
        assertLuaEqual (pushRealFloat @Float 0.0) "'0.0'"
      , "42.0" =:
        assertLuaEqual (pushRealFloat @Float 42.0) "'42.0'"
      , "-0.00071" =:
        assertLuaEqual (pushRealFloat @Float (-0.00071)) "'-7.1e-4'"
      , "-13.37" =:
        assertLuaEqual (pushRealFloat @Float (-13.37)) "'-13.37'"
      , testSingleElementProperty (pushRealFloat @Float)
      ]
    ]

  , testGroup "Strings"
    [ testGroup "pushByteString"
      [ "\"test\"" =:
        assertLuaEqual (pushByteString "test") "\"test\""
      , testSingleElementProperty pushByteString
      ]

    , testGroup "pushString"
      [ "\"test\"" =:
        assertLuaEqual (pushString "test") "\"test\""
      , "unicode" =:
        assertLuaEqual (pushString "ÄÉÏøûßð") (Utf8.fromString "'ÄÉÏøûßð'")
      , testSingleElementProperty pushString
      ]

    , testGroup "pushText"
      [ "\"test\"" =:
        assertLuaEqual (pushText "test") "\"test\""
      , "unicode" =:
        assertLuaEqual (pushText "ÄÉÏøûßð") (Utf8.fromString "'ÄÉÏøûßð'")
      , testSingleElementProperty pushText
      ]
    ]

  , testGroup "Collections"
    [ testGroup "pushList"
      [ testProperty "creates a table" $ \x -> monadicIO $ do
          producesTable <- run $ Lua.run $ do
            pushList pushBool x
            listType <- Lua.ltype Lua.stackTop
            return $ Lua.TypeTable == listType
          assert producesTable

      , testProperty "numeric indices start at 1" $ \list -> monadicIO $ do
          retrievedList <- run $ Lua.run $ do
            pushList (pushIntegral @Lua.Integer) list
            listIdx <- Lua.absindex Lua.stackTop
            forM [1..(fromIntegral $ length list)] $ \n ->
              Lua.rawgeti listIdx n
              *> (fromMaybe 0 <$> Lua.tointeger Lua.stackTop)
              <* Lua.pop 1
          assert $ retrievedList == list

      , testProperty "table size equals list length" $ \list -> monadicIO $ do
          tableSize <- run $ Lua.run $ do
            pushList pushString list
            Lua.rawlen Lua.stackTop
          assert $ tableSize == length list

      , testSingleElementProperty (pushList pushText)
      ]

    , testGroup "pushSet"
      [ testProperty "creates a table" $ \x -> monadicIO $ do
          producesTable <- run $ Lua.run $ do
            pushSet pushString x
            listType <- Lua.ltype Lua.stackTop
            return $ Lua.TypeTable == listType
          assert producesTable

      , testProperty "set values become table keys" $ \set -> monadicIO $
          case Set.lookupMin set of
            Nothing -> return ()
            Just el -> do
              hasKey <- run $ Lua.run $ do
                pushSet (pushIntegral @Lua.Integer) set
                pushIntegral el
                Lua.gettable (Lua.nthFromTop 2)
                Lua.toboolean Lua.stackTop
              assert hasKey

      , testSingleElementProperty (pushSet pushText)
      ]

    , testGroup "pushMap"
      [ testProperty "creates a table" $ \m -> monadicIO $ do
          producesTable <- run $ Lua.run $ do
            pushMap pushString pushString m
            listType <- Lua.ltype Lua.stackTop
            return $ Lua.TypeTable == listType
          assert producesTable

      , testProperty "pairs are in table" $ \m -> monadicIO $
          case Map.lookupMax m of
            Nothing -> return ()
            Just (k, v) -> do
              tabVal <- run $ Lua.run $ do
                pushMap pushText (pushRealFloat @Lua.Number) m
                pushText k
                Lua.gettable (Lua.nthFromTop 2)
                fromMaybe (error "key not found") <$>
                  Lua.tonumber Lua.stackTop
              assert (tabVal == v)

      , testSingleElementProperty (pushMap pushText (pushRealFloat @Double))
      ]
    ]
  ]

-- | Executes a Lua action and checks whether a the value at the top of the
-- stack equals the value represented by the string.
assertLuaEqual :: Lua () -> ByteString -> Assertion
assertLuaEqual action lit =
  let comparison = Lua.run $ do
        action
        pushLuaExpr lit
        isSame <- Lua.rawequal (Lua.nthFromTop 1) (Lua.nthFromTop 2)
        if isSame
          then return Nothing
          else do
            expectedType <- Lua.ltype (Lua.nthFromTop 1) >>= Lua.typename
            actualType <- Lua.ltype (Lua.nthFromTop 2) >>= Lua.typename
            actual <- Lua.tostring' (Lua.nthFromTop 2)
            return . Just $
              "Expected '" <> Utf8.toString lit <> "' (" <> expectedType <>
              ") but got '" <> Utf8.toString actual <> "'" <>
              " (" <> actualType <> ")"
  in comparison >>= \case
    Nothing -> return ()
    Just err -> assertFailure err


-- | Verifies that the operation adds exactly one element to the Lua stack.
testSingleElementProperty :: (Arbitrary a, Show a) => Pusher a -> TestTree
testSingleElementProperty push = testProperty "pushes single element" $ \x ->
  monadicIO $ do
    (oldSize, newSize) <- run . Lua.run $ do
      old <- Lua.gettop
      push x
      new <- Lua.gettop
      return (old, new)
    assert (newSize == succ oldSize)
