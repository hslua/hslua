{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Marshalling.PeekersTests
Copyright   : © 2020-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for Haskell-value retriever functions.
-}
module HsLua.Marshalling.PeekersTests (tests) where

import Control.Monad (forM_, zipWithM_)
import HsLua.Marshalling.Peek
import HsLua.Marshalling.Peekers

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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "Peekers"
  [ testGroup "unit peekers"
    [ "peekNil succeeds on nil" =:
      Success () `shouldBeResultOf` do
        Lua.pushnil
        runPeek $ peekNil Lua.top
    , "peekNil fails on bool" =:
      isFailure `shouldHoldForResultOf` do
        Lua.pushboolean False
        runPeek $ peekNil Lua.top
    , "peekNoneOrNil succeeds on nil" =:
      Success () `shouldBeResultOf` do
        Lua.pushnil
        runPeek $ peekNoneOrNil Lua.top
    , "peekNoneOrNilfails on bool" =:
      isFailure `shouldHoldForResultOf` do
        Lua.pushboolean False
        runPeek $ peekNoneOrNil Lua.top
    ]
  , testGroup "peekBool"
    [ "True" =:
      Success True `shouldBeResultOf` do
        Lua.pushboolean True
        runPeeker peekBool Lua.top

    , "False" =:
      Success False `shouldBeResultOf` do
        Lua.pushboolean False
        runPeeker peekBool Lua.top

    , "Numbers are truthy" =:
      Success True `shouldBeResultOf` do
        Lua.pushnumber 0
        runPeeker peekBool Lua.top

    , "Nil is falsy" =:
      Success False `shouldBeResultOf` do
        Lua.pushnil
        runPeeker peekBool Lua.top

    -- no tests for failing cases, this function always succeeds.
    ]

  , testGroup "peekIntegral"
    [ "negative Int" =:
      Success (-5) `shouldBeResultOf` do
        Lua.pushinteger (-5)
        runPeek $ peekIntegral @Int Lua.top

    , "Int as string" =:
      Success 720 `shouldBeResultOf` do
        Lua.pushstring "720"
        runPeek $ peekIntegral @Int Lua.top

    , "fail on boolean" =:
      let msg = "Integral expected, got boolean"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        runPeek $ peekIntegral @Int Lua.top

    , "fail on non-numeric string" =:
      let msg = "Integral expected, got string"
      in failure msg `shouldBeResultOf` do
        Lua.pushstring "not a number"
        runPeek $ peekIntegral @Integer Lua.top
    ]

  , testGroup "peekRealFloat"
    [ "negative Float" =:
      Success (-13.37) `shouldBeResultOf` do
        Lua.pushnumber (-13.37)
        runPeek $ peekRealFloat @Float Lua.top

    , "number as string" =:
      Success (-720.0) `shouldBeResultOf` do
        Lua.pushstring "-720"
        runPeek $ peekRealFloat @Float Lua.top

    , "scientific notation string" =:
      Success 0.00071 `shouldBeResultOf` do
        Lua.pushstring "7.1e-4"
        runPeek $ peekRealFloat @Float Lua.top

    , "fail on boolean" =:
      let msg = "RealFloat expected, got boolean"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        runPeek $ peekRealFloat @Float Lua.top

    , "fail on non-numeric string" =:
      let msg = "RealFloat expected, got string"
      in failure msg `shouldBeResultOf` do
        Lua.pushstring "not a number"
        runPeek $ peekRealFloat @Double Lua.top
    ]

  , testGroup "Strings"
    [ testGroup "peekByteString"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring bs
            runPeeker peekByteString Lua.top
          assert (retrieved == Success bs)

      , testProperty "retrieve integer as string" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run @Lua.Exception $ do
            Lua.pushinteger n
            runPeeker peekByteString Lua.top
          let numberAsByteString = Char8.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Success numberAsByteString)

      , "fails on boolean" =:
      let msg = "string expected, got boolean"
      in failure msg `shouldBeResultOf` do
        Lua.pushboolean True
        runPeeker peekByteString Lua.top
      ]

    , testGroup "peekText"
      [ testProperty "retrieve any string" $ \bs -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring bs
            runPeeker peekText Lua.top
          assert (retrieved == Success (Utf8.toText bs))

      , testProperty "retrieve UTF-8 encoded Text" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring (Utf8.fromText txt)
            runPeeker peekText Lua.top
          assert (retrieved == Success txt)

      , testProperty "retrieve integer as Text" $ \n -> monadicIO $ do
          retrieved <- run . Lua.run @Lua.Exception $ do
            Lua.pushinteger n
            runPeeker peekText Lua.top
          let numberAsByteString = T.pack . show @Integer . fromIntegral $ n
          assert (retrieved == Success numberAsByteString)

      , "fails on nil" =:
        let msg = "string expected, got nil"
        in failure msg `shouldBeResultOf` do
          Lua.pushnil
          runPeeker peekByteString Lua.top
      ]

    , testGroup "peekString"
      [ testProperty "retrieve UTF-8 encoded string" $ \txt -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring (Utf8.fromString txt)
            runPeeker peekString Lua.top
          assert (retrieved == Success txt)

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          runPeeker peekString Lua.top

      , "fails on thread" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushthread
          runPeeker peekString Lua.top
      ]

    , testGroup "peekStringy"
      [ testProperty "retrieve UTF-8 encoded string as Text" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run @Lua.Exception $ do
              Lua.pushstring (Utf8.fromText txt)
              runPeeker (peekStringy @T.Text) Lua.top
            assert (retrieved == Success txt)

      , "retrieve ByteString" =:
        Success "This is an ASCII string" `shouldBeResultOf` do
          Lua.pushstring "This is an ASCII string"
          runPeeker (peekStringy @B.ByteString) Lua.top

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          runPeeker (peekStringy @B.ByteString) Lua.top
      ]

    , testGroup "peekName"
      [ testProperty "retrieve string as Name" $ \txt ->
          monadicIO $ do
            retrieved <- run $ Lua.run @Lua.Exception $ do
              Lua.pushstring txt
              runPeeker peekName Lua.top
            assert (retrieved == Success (Lua.Name txt))

      , "fails on table" =:
        isFailure `shouldHoldForResultOf` do
          _ <- Lua.pushglobaltable
          runPeeker peekName Lua.top
      ]
    ]

  , testGroup "peekRead"
    [ testProperty "retrieve list of orderings" $ \xs ->
        monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.pushstring . Utf8.fromString $ show @[Ordering] xs
            runPeeker peekRead Lua.top
          assert (retrieved == Success xs)

    , "fails on unreadable input" =:
      isFailure `shouldHoldForResultOf` do
        Lua.pushstring "NaN"
        runPeek $ peekRead @Int Lua.top

    , "fails on non-string input" =:
      "string expected, got boolean" `shouldBeErrorMessageOf` do
        Lua.pushboolean True
        runPeeker (peekRead @Int) Lua.top >>= force
    ]

  , testGroup "Containers"
    [ testGroup "peekList"
      [ "empty list" =:
        Success [] `shouldBeResultOf` do
          Lua.newtable
          runPeek $ peekList peekBool Lua.top

      , testProperty "list of strings" $ \lst -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.newtable
            zipWithM_
              (\i s -> Lua.pushstring s *>
                       Lua.rawseti (Lua.nth 2) i)
              [1..]
              lst
            runPeek $ peekList peekByteString Lua.top
          assert (retrieved == Success lst)

      , "string keys are not in list" =:
        Success [] `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = 'world'}"
          runPeek $ peekList peekByteString Lua.top

      , "missing pair causes an error" =:
        isFailure `shouldHoldForResultOf` do
          pushLuaExpr "{[1] = 'hello', [2] = 'world', [4] = 'nope'}"
          runPeek $ peekList peekByteString Lua.top
      ]

    , testGroup "peekNonEmpty"
      [ "empty list" =:
        Failure "empty list" ["retrieving NonEmpty"] `shouldBeResultOf` do
          Lua.newtable
          runPeek $ peekNonEmpty peekBool Lua.top

      , "non-empty list" =:
        Success (5 NonEmpty.:| [23]) `shouldBeResultOf` do
          pushLuaExpr "{ 5, 23 }"
          runPeek $ peekNonEmpty (peekIntegral @Int) Lua.top
      ]

    , testGroup "peekSet"
      [ "empty set" =:
        Success Set.empty `shouldBeResultOf` do
          Lua.newtable
          runPeek $ peekSet peekBool Lua.top

      , testProperty "set of strings" $ \set -> monadicIO $ do
          retrieved <- run $ Lua.run @Lua.Exception $ do
            Lua.newtable
            forM_ (Set.toList set) $ \x -> do
              Lua.pushstring x
              Lua.pushboolean True
              Lua.rawset (Lua.nth 3)
            runPeek $ peekSet peekByteString Lua.top
          assert (retrieved == Success set)

      , "keys with falsy values are not in set" =:
        Success (Set.fromList [1,3]) `shouldBeResultOf` do
          pushLuaExpr "{['1'] = 'hello', ['2'] = false, [3] = 5}"
          runPeek $ peekSet (peekIntegral @Int) Lua.top

      , "fails if element peeker fails" =:
        let errorStack = [ "retrieving Set"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         ]
            errorMsg = "string expected, got boolean"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true, [true] = false }"
          runPeek $ peekSet peekText Lua.top
      ]

    , testGroup "peekMap"
      [ "empty map" =:
        Success Map.empty `shouldBeResultOf` do
          Lua.newtable
          runPeek $ peekMap peekText peekText Lua.top

      , "tables become maps" =:
        Success (Map.fromList [("one", 1), ("two", 2)]) `shouldBeResultOf` do
          pushLuaExpr "{ one = 1, two = 2}"
          runPeek $ peekMap peekText (peekIntegral @Int) Lua.top

      , "fails if key peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving key"
                         ]
            errorMsg = "Integral expected, got string"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ NaN = true }"
          runPeek $ peekMap (peekIntegral @Int) peekBool Lua.top

      , "fails if value peeker fails" =:
        let errorStack = [ "retrieving Map"
                         , "retrieving key-value pair"
                         , "retrieving value"
                         ]
            errorMsg = "string expected, got boolean"
        in Failure errorMsg errorStack `shouldBeResultOf` do
          pushLuaExpr "{ [42] = true }"
          runPeek $ peekMap (peekIntegral @Int) peekText Lua.top
      ]
    ]


  , testGroup "combinators"
    [ testGroup "peekFieldRaw"
      [ "access field" =:
        Success 8 `shouldBeResultOf` do
          pushLuaExpr "{ num = 8 }"
          runPeek $ peekFieldRaw (peekIntegral @Int) "num" Lua.top
      , "object not on top of stack" =:
        Success 9 `shouldBeResultOf` do
          pushLuaExpr "{ int = 9 }"
          Lua.pushnil
          runPeek $ peekFieldRaw (peekIntegral @Int) "int" (Lua.nth 2)
      ]

    , testGroup "peekPair"
      [ "pair from table" =:
        Success ("ninety", 90) `shouldBeResultOf` do
          pushLuaExpr "{'ninety', 90}"
          Lua.pushnil
          runPeek $
            peekPair peekString (peekIntegral @Int) (Lua.nth 2)

      , "fails if a component peeker fails" =:
        Failure "fail" [] `shouldBeResultOf` do
          pushLuaExpr "{ 'second', 2 }"
          runPeek $
            peekPair peekString (const $ failPeek @() "fail") Lua.top
      ]

    , testGroup "peekTriple"
      [ "pair from table" =:
        Success ("hundred", 100, True) `shouldBeResultOf` do
          pushLuaExpr "{'hundred', 100, 1}"
          Lua.pushnil
          runPeek $
            peekTriple peekString (peekIntegral @Int) peekBool (Lua.nth 2)

      , "fails if a component peeker fails" =:
        Failure "fail" [] `shouldBeResultOf` do
          pushLuaExpr "{ 'second', 2, true }"
          runPeek $
            peekTriple peekString
                       (const $ failPeek @() "fail")
                       peekBool
                       Lua.top
      ]
    , testGroup "peekChoice"
      [ "uses first result to succeed" =:
        Success 1337 `shouldBeResultOf` runPeek
          (choice [ const $ failPeek "nope"
                  , const $ failPeek "neither"
                  , const $ return (1337 :: Int)
                  ] Lua.top)

      , "uses peekers" =:
        Success "[]" `shouldBeResultOf` do
          Lua.newtable
          runPeeker
            (choice [ peekString
                    , fmap show . peekList peekBool
                    ])
            Lua.top

      , "fails if all peekers fail" =:
        Failure "all choices failed" [] `shouldBeResultOf` do
          runPeeker (choice [const $ failPeek @() "nope"]) Lua.top
      ]

    , testGroup "peekNilOr"
      [ "returns the parser result if the value is not nil" =:
        Success (Just "a") `shouldBeResultOf` runPeek
          (liftLua (Lua.pushstring "a") *> peekNilOr peekString Lua.top)
      , "returns nothing if the value is nil" =:
        Success Nothing `shouldBeResultOf` runPeek
          (liftLua Lua.pushnil *> peekNilOr peekString Lua.top)
      , "fails if the value is none" =:
        Failure "string expected, got no value" [] `shouldBeResultOf` runPeek
          (liftLua Lua.gettop >>= peekNilOr @Maybe peekString . (+1))
      ]

    , testGroup "peekNoneOr"
      [ "returns the parser result if a value is present" =:
        Success (Just "a") `shouldBeResultOf` runPeek
          (liftLua (Lua.pushstring "a") *> peekNoneOr peekString Lua.top)
      , "returns the parser result if the value is nil" =:
        Success (Just ()) `shouldBeResultOf` runPeek
          (liftLua Lua.pushnil *> peekNoneOr peekNil Lua.top)
      , "returns `empty` if the value is missing" =:
        Success Nothing `shouldBeResultOf` runPeek
          (liftLua Lua.gettop >>= peekNoneOr @Maybe peekString . (+1))
      , "fails if the parser cannot parse the value" =:
        Failure "string expected, got nil" [] `shouldBeResultOf` runPeek
          (liftLua Lua.pushnil *> peekNoneOr @Maybe peekString Lua.top)
      ]
    , testGroup "peekNoneOrNilOr"
      [ "returns the parser result if a value is present" =:
        Success (Just "a") `shouldBeResultOf` runPeek
          (liftLua (Lua.pushstring "a") *> peekNoneOrNilOr peekString Lua.top)
      , "returns `empty` if the value is nil" =:
        Success Nothing `shouldBeResultOf` runPeek
          (liftLua Lua.pushnil *> peekNoneOrNilOr @Maybe peekString Lua.top)
      , "returns `empty` if the value is missing" =:
        Success Nothing `shouldBeResultOf` runPeek
          (liftLua Lua.gettop >>= peekNoneOrNilOr @Maybe peekString . (+1))
      , "fails if the parser cannot parse the value" =:
        Failure "string expected, got boolean" [] `shouldBeResultOf` runPeek
          (liftLua (Lua.pushboolean True) *>
           peekNoneOrNilOr @Maybe peekString Lua.top)
      ]
    ]

  , testGroup "helper"
    [ testGroup "reportValueOnFailure"
      [ "success" =:
        Success 23 `shouldBeResultOf` do
          runPeeker
            (reportValueOnFailure "foo" (const . return $ Just (23 :: Int)))
            (Lua.nthBottom 1)

      , "failure" =:
        Failure "squirrel expected, got number" []
        `shouldBeResultOf` do
          Lua.pushinteger 23
          let peekSquirrel :: Peeker Lua.Exception ()
              peekSquirrel = reportValueOnFailure "squirrel"
                                                  (const $ return Nothing)
          runPeeker peekSquirrel Lua.top
      ]
    ]

  , testGroup "error messages"
    [ "value in list" =:
      mconcat ["Integral expected, got table\n"
              , "\twhile retrieving index 3\n"
              , "\twhile retrieving list"
              ] `shouldBeErrorMessageOf` do
        Lua.openlibs
        Lua.OK <- Lua.dostring $ Utf8.fromString
          "nope = {__tostring = function () return '⚘' end}"
        pushLuaExpr "{5, 8, setmetatable({}, nope), 21}"
        runPeeker (peekList (peekIntegral @Int)) Lua.top >>= force

    , "nil instead of list" =:
      mconcat ["table expected, got nil\n"
              , "\twhile retrieving list"
              ] `shouldBeErrorMessageOf` do
        Lua.pushnil
        runPeeker (peekList peekString) Lua.top >>= force

    , "value in key-value pairs" =:
      mconcat [ "string expected, got boolean\n"
              , "\twhile retrieving value\n"
              , "\twhile retrieving key-value pair"
              ] `shouldBeErrorMessageOf` do
        pushLuaExpr "{ a = true}"
        runPeeker (peekKeyValuePairs peekText peekText) Lua.top >>= force
    ]
  ]
