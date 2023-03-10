{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Main
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for type specifiers.
-}
import Control.Monad (when)
import Data.String (fromString)
import HsLua.Core
import HsLua.Core.Types
import HsLua.Marshalling
import HsLua.Typing
import Lua.Arbitrary ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()
import qualified HsLua.Core as HsLua
import qualified Test.QuickCheck.Monadic as QC

-- | Run this spec.
main :: IO ()
main = defaultMain tests

-- | Aeson tests
tests :: TestTree
tests = testGroup "hslua-typespec"
  [ testGroup "string representation"
    [ testCase "any"      $ "any"      @?= anyType
    , testCase "boolean"  $ "boolean"  @?= booleanType
    , testCase "function" $ "function" @?= functionType
    , testCase "integer"  $ "integer"  @?= integerType
    , testCase "nil"      $ "nil"      @?= nilType
    , testCase "number"   $ "number"   @?= numberType
    , testCase "string"   $ "string"   @?= stringType
    , testCase "table"    $ "table"    @?= tableType
    , testCase "userdata" $ "userdata" @?= userdataType
    ]

  , testGroup "to string"
    [ testCase "any"      $ typeSpecToString anyType      @?= "any"
    , testCase "boolean"  $ typeSpecToString booleanType  @?= "boolean"
    , testCase "function" $ typeSpecToString functionType @?= "function"
    , testCase "number"   $ typeSpecToString numberType   @?= "number"
    , testCase "string"   $ typeSpecToString stringType   @?= "string"
    , testCase "table"    $ typeSpecToString tableType    @?= "table"
    , testCase "userdata" $ typeSpecToString userdataType @?= "userdata"
    , testCase "sequence" $
      seqType stringType @?= "{string,...}"
    ]

  , testGroup "operators"
    [ testGroup "#|#"
      -- These should be property tests
      [ testCase "combining basic types yields sum type" $
        booleanType #|# numberType @?= SumType [booleanType, numberType]
      , testCase "any is the unit" $ do
        booleanType #|# anyType @?= anyType
        anyType #|# booleanType @?= anyType
      , testCase "void is zero" $ do
        booleanType #|# voidType @?= booleanType
        voidType #|# numberType @?= numberType
      ]
    ]

  , testGroup "Marshalling"
    [ testProperty "Roundtrip TypeSpec" $
      assertRoundtripEqual pushTypeSpec peekTypeSpec

    , testProperty "Roundtrip TypeDocs" $
      assertRoundtripEqual pushTypeDoc peekTypeDoc
    ]
  ]

instance Arbitrary TypeSpec where
  arbitrary = arbitraryTypeSpec 3
  shrink = shrinkTypeSpec

instance Arbitrary TypeDocs where
  arbitrary = TypeDocs
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink td = (\ts -> td{ typeSpec = ts}) <$> shrink (typeSpec td)

instance Arbitrary Name where
  arbitrary = Name . fromString <$> arbitrary

arbitraryTypeSpec :: Int -> Gen TypeSpec
arbitraryTypeSpec size = frequency
  [ (8, BasicType . toType <$> arbitrary)
  , (1, NamedType <$> arbitrary)
  , (3, resize (size - 1) $ SeqType <$> arbitrary)
  , (2, resize (size - 1) $ SumType <$> arbitrary)
  , (2, resize (size - 1) $ RecType <$> arbitrary)
  , (1, resize (size - 1) $ FunType <$> arbitrary <*> arbitrary)
  , (1, return AnyType)
  ]

shrinkTypeSpec :: TypeSpec -> [TypeSpec]
shrinkTypeSpec = \case
  NamedType n -> typeSpec n : (NamedType <$> shrink n)
  SumType cs  -> SumType <$> shrinkList shrink cs
  SeqType x   -> shrink x
  FunType d c -> (FunType c <$> shrinkList shrinkTypeSpec d) ++
                 ((`FunType` d)  <$> shrinkList shrinkTypeSpec c)
  x -> shrinkNothing x


assertRoundtripEqual :: Eq a
                     => Pusher HsLua.Exception a -> Peeker HsLua.Exception a
                     -> a -> Property
assertRoundtripEqual pushX peekX x = QC.monadicIO $ do
  y <- QC.run $ roundtrip pushX peekX x
  QC.assert (x == y)

roundtrip :: Pusher HsLua.Exception a -> Peeker HsLua.Exception a -> a -> IO a
roundtrip pushX peekX x = run $ do
  pushX x
  size <- gettop
  when (size /= 1) $
    failLua $ "not the right amount of elements on the stack: " ++ show size
  result <- forcePeek $ peekX top
  afterPeekSize <- gettop
  when (afterPeekSize /= 1) $
    failLua $ "peeking modified the stack: " ++ show afterPeekSize
  return result
