{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Marshalling.PeekTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : OverloadedStrings, TypeApplications

Tests for Haskell-value retriever functions.
-}
module HsLua.Marshalling.PeekTests (tests) where

import Control.Monad (forM_, zipWithM_)
import Data.Maybe (fromMaybe)
import HsLua.Marshalling.Peek

import Lua.Arbitrary ()
import Test.Tasty.HsLua
  ( (=:), pushLuaExpr, shouldBeResultOf, shouldHoldForResultOf
  , shouldBeErrorMessageOf)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Peek"
  [ testGroup "peekBool"
    [ "True" =:
      Success True `shouldBeResultOf` do
        Lua.pushboolean True
        peekBool Lua.top

    , "False" =:
      Success False `shouldBeResultOf` do
        Lua.pushboolean False
        peekBool Lua.top

    , "Numbers are truthy" =:
      Success True `shouldBeResultOf` do
        Lua.pushnumber 0
        peekBool Lua.top

    , "Nil is falsy" =:
      Success False `shouldBeResultOf` do
        Lua.pushnil
        peekBool Lua.top

    -- no tests for failing cases, this function always succeeds.
    ]

  , testGroup "peekIntegral"
    [ "negative Int" =:
      Success (-5) `shouldBeResultOf` do
        Lua.pushinteger (-5)
        peekIntegral @Int Lua.top

    , "Int as string" =:
      Success 720 `shouldBeResultOf` do
        Lua.pushstring "720"
        peekIntegral @Int Lua.top

    , "fail on boolean" =:
      let msg = "expected Integral, got 'true' (boolean)"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        peekIntegral @Int Lua.top

    , "fail on non-numeric string" =:
      let msg = "expected Integral, got 'not a number' (string)"
      in failure msg `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekIntegral @Integer Lua.top
    ]

  , testGroup "peekRealFloat"
    [ "negative Float" =:
      Success (-13.37) `shouldBeResultOf` do
        Lua.pushnumber (-13.37)
        peekRealFloat @Float Lua.top

    , "number as string" =:
      Success (-720.0) `shouldBeResultOf` do
        Lua.pushstring "-720"
        peekRealFloat @Float Lua.top

    , "scientific notation string" =:
      Success 0.00071 `shouldBeResultOf` do
        Lua.pushstring "7.1e-4"
        peekRealFloat @Float Lua.top

    , "fail on boolean" =:
      let msg = "expected RealFloat, got 'true' (boolean)"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        peekRealFloat @Float Lua.top

    , "fail on non-numeric string" =:
      let msg = "expected RealFloat, got 'not a number' (string)"
      in failure msg `shouldBeResultOf` do
        Lua.pushstring "not a number"
        peekRealFloat @Double Lua.top
    ]

  , testGroup "Strings"
    [ testGroup "peekByteString"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring bs
            peekByteString Lua.top
          assert (retrieved == Success bs)

      , testProperty "retrieve integer as string" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run @Lua.Exception $ do
            Lua.pushinteger n
            peekByteString Lua.top
          let numberAsByteString = Char8.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Success numberAsByteString)

      , "fails on boolean" =:
      let msg = "expected string, got 'true' (boolean)"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        peekByteString Lua.top
      ]

    , testGroup "peekText"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring bs
            peekText Lua.top
          assert (retrieved == Success (Utf8.toText bs))

      , testProperty "retrieve UTF-8 encoded Text" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring (Utf8.fromText txt)
            peekText Lua.top
          assert (retrieved == Success txt)

      , testProperty "retrieve integer as Text" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run @Lua.Exception $ do
            Lua.pushinteger n
            peekText Lua.top
          let numberAsByteString = T.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Success numberAsByteString)

      , "fails on nil" =:
        let msg = "expected string, got 'nil' (nil)"
        in failure msg `shouldBeResultOf` do
          Lua.pushnil
          peekByteString Lua.top
      ]

    , testGroup "peekString"
      [ testProperty "retrieve UTF-8 encoded string" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring (Utf8.fromString txt)
            peekString Lua.top
          assert (retrieved == Success txt)

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekString Lua.top

      , "fails on thread" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushthread
          peekString Lua.top
      ]

    , testGroup "peekStringy"
      [ testProperty "retrieve UTF-8 encoded string as Text" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run @Lua.Exception $ do
              Lua.pushstring (Utf8.fromText txt)
              peekStringy @T.Text Lua.top
            assert (retrieved == Success txt)

      , "retrieve ByteString" =:
        Success "This is an ASCII string" `shouldBeResultOf` do
          Lua.pushstring "This is an ASCII string"
          peekStringy @B.ByteString Lua.top

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekStringy @B.ByteString Lua.top
      ]

    , testGroup "peekName"
      [ testProperty "retrieve string as Name" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run @Lua.Exception $ do
              Lua.pushstring txt
              peekName Lua.top
            assert (retrieved == Success (Lua.Name txt))

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          peekName Lua.top
      ]
    ]

  , testGroup "peekRead"
    [ testProperty "retrieve list of orderings" $ \xs ->
        monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring . Utf8.fromString $ show @[Ordering] xs
            peekRead Lua.top
          assert (retrieved == Success xs)

    , "fails on unreadable input" =:
      isFailure `shouldHoldForResultOf` do
        Lua.pushstring "NaN"
        peekRead @Int Lua.top

    , "fails on non-string input" =:
      "expected string, got 'true' (boolean)" `shouldBeErrorMessageOf` do
        Lua.pushboolean True
        peekRead @Int Lua.top >>= force
    ]

  , testGroup "Containers"
    [ testGroup "peekList"
      [ "empty list" =:
        Success [] `shouldBeResultOf` do
          Lua.newtable
          peekList peekBool Lua.top

      , testProperty "list of strings" $ \lst -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.newtable
            zipWithM_
              (\i s -> Lua.pushstring s *>
                       Lua.rawseti (Lua.nth 2) i)
              [1..]
              lst
            peekList peekByteString Lua.top
          assert (retrieved == Success lst)

      , "string keys are not in list" =:
        Success [] `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = 'world'}"
          peekList peekByteString Lua.top

      , "missing pair causes an error" =:
        isFailure `shouldHoldForResultOf` do
          pushLuaExpr "{[1] = 'hello', [2] = 'world', [4] = 'nope'}"
          peekList peekByteString Lua.top
      ]

    , testGroup "peekSet"
      [ "empty set" =:
        Success Set.empty `shouldBeResultOf` do
          Lua.newtable
          peekSet peekBool Lua.top

      , testProperty "set of strings" $ \set -> monadicIO $ do
          retrieved <- run $ Lua.run $ do
            Lua.newtable
            forM_ (Set.toList set) $ \x -> do
              Lua.pushstring x
              Lua.pushboolean True
              Lua.rawset (Lua.nth 3)
            peekSet @Lua.Exception peekByteString Lua.top
          assert (retrieved == Success set)

      , "keys with falsy values are not in set" =:
        Success (Set.fromList [1,3]) `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = false, [3] = 5}"
          peekSet (peekIntegral @Int) Lua.top

      , "fails if element peeker fails" =:
        let errorStack = [ "Set", "key-value pair", "key"]
            errorMsg = "expected string, got 'true' (boolean)"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true, [true] = false }"
          peekSet peekText Lua.top
      ]

    , testGroup "peekMap"
      [ "empty map" =:
        Success Map.empty `shouldBeResultOf` do
          Lua.newtable
          peekMap peekText peekText Lua.top

      , "tables become maps" =:
        Success (Map.fromList [("one", 1), ("two", 2)]) `shouldBeResultOf` do
          pushLuaExpr "{ one = 1, two = 2}"
          peekMap peekText (peekIntegral @Int) Lua.top

      , "fails if key peeker fails" =:
        let errorStack = [ "Map", "key-value pair" , "key" ]
            errorMsg = "expected Integral, got 'NaN' (string)"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true }"
          peekMap (peekIntegral @Int) peekBool Lua.top

      , "fails if value peeker fails" =:
        let errorStack = [ "Map", "key-value pair", "value" ]
            errorMsg = "expected string, got 'true' (boolean)"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ [42] = true }"
          peekMap (peekIntegral @Int) peekText Lua.top
      ]
    ]

  , testGroup "combinators"
    [ "optional with nil" =:
      Success Nothing `shouldBeResultOf` do
        Lua.pushnil
        optional peekString Lua.top
    , "optional with number" =:
      Success (Just 23) `shouldBeResultOf` do
        Lua.pushinteger @Lua.Exception 23
        optional (peekIntegral @Int) Lua.top

    , testGroup "peekFieldRaw"
      [ "access field" =:
        Success 8 `shouldBeResultOf` do
          pushLuaExpr "{ num = 8 }"
          peekFieldRaw (peekIntegral @Int) "num" Lua.top
      , "object not on top of stack" =:
        Success 9 `shouldBeResultOf` do
          pushLuaExpr "{ int = 9 }"
          Lua.pushnil
          peekFieldRaw (peekIntegral @Int) "int" (Lua.nth 2)
      ]

    , testGroup "peekPair"
      [ "pair from table" =:
        Success ("ninety", 90) `shouldBeResultOf` do
          pushLuaExpr "{'ninety', 90}"
          Lua.pushnil
          peekPair (peekString, peekIntegral @Int) (Lua.nth 2)

      , "fails if a component peeker fails" =:
        Failure "fail" [] `shouldBeResultOf` do
          pushLuaExpr "{ 'second', 2 }"
          peekPair (peekString, const $ return (failure @() "fail")) Lua.top
      ]

    , testGroup "peekTriple"
      [ "pair from table" =:
        Success ("hundred", 100, True) `shouldBeResultOf` do
          pushLuaExpr "{'hundred', 100, 1}"
          Lua.pushnil
          peekTriple (peekString, peekIntegral @Int, peekBool) (Lua.nth 2)

      , "fails if a component peeker fails" =:
        Failure "fail" [] `shouldBeResultOf` do
          pushLuaExpr "{ 'second', 2, true }"
          peekTriple ( peekString
                     , const $ return (failure @() "fail")
                     , peekBool)
            Lua.top
      ]
    , testGroup "peekChoice"
      [ "uses first result to succeed" =:
        Success 1337 `shouldBeResultOf` do
          choice [ const $ return (failure @Int "nope")
                 , const $ return (failure @Int "neither")
                 , const $ return (Success 1337)
                 ] Lua.top

      , "uses peekers" =:
        Success "[]" `shouldBeResultOf` do
          Lua.newtable
          choice [ peekString
                 , fmap (fmap show) . peekList peekBool
                 ]
            Lua.top

      , "fails if all peekers fail" =:
        Failure "all choices failed" [] `shouldBeResultOf` do
          choice [const $ return (failure @() "nope")] Lua.top
      ]
    ]

  , testGroup "helper"
    [ testGroup "reportValueOnFailure"
      [ "success" =:
        Success 23 `shouldBeResultOf` do
          reportValueOnFailure "foo" (const . return $ Just (23 :: Int))
            (Lua.nthBottom 1)

      , "failure" =:
        Failure "expected squirrel, got '23' (number)" []
        `shouldBeResultOf` do
          Lua.pushinteger 23
          let peekSquirrel :: Peeker Lua.Exception ()
              peekSquirrel = reportValueOnFailure "squirrel"
                                                  (const $ return Nothing)
          peekSquirrel Lua.top
      ]

    , "retrieving" =:
      Failure "message" ["context"] `shouldBeResultOf` do
        retrieving "context" . return $ failure @() "message"

    , let firstindex idx = do
            Lua.rawgeti idx 1
            fromMaybe 0 <$> Lua.tointeger Lua.top <* Lua.pop 1
      in testGroup "toPeeker"
      [ "passes result through" =:
        Success 1337 `shouldBeResultOf` do
          pushLuaExpr "{1337}"
          toPeeker firstindex Lua.top

      , "catches error" =:
        let msg = "Lua exception: expected table, got '1337' (number)"
        in
          Failure msg [] `shouldBeResultOf` do
          Lua.pushinteger 1337
          toPeeker firstindex Lua.top
      ]
    ]

  , testGroup "error messages"
    [ "value in list" =:
      mconcat ["expected Integral, got '⚘' (table)\n"
              , "\twhile retrieving index 3\n"
              , "\twhile retrieving list"
              ] `shouldBeErrorMessageOf` do
        Lua.openlibs
        Lua.OK <- Lua.dostring $ Utf8.fromString
          "nope = {__tostring = function () return '⚘' end}"
        pushLuaExpr "{5, 8, setmetatable({}, nope), 21}"
        peekList (peekIntegral @Int) Lua.top >>= force

    , "nil instead of list" =:
      mconcat ["expected table, got 'nil' (nil)\n"
              , "\twhile retrieving list"
              ] `shouldBeErrorMessageOf` do
        Lua.pushnil
        peekList peekString Lua.top >>= force

    , "value in key-value pairs" =:
      mconcat [ "expected string, got 'true' (boolean)\n"
              , "\twhile retrieving value\n"
              , "\twhile retrieving key-value pair"
              ] `shouldBeErrorMessageOf` do
        pushLuaExpr "{ a = true}"
        peekKeyValuePairs peekText peekText Lua.top >>= force
    ]
  ]
