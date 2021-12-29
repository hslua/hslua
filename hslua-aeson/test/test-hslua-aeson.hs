{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   :  © 2017–2021 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (when)
import Data.AEq ((~==))
import Data.ByteString (ByteString)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.Aeson
import Test.QuickCheck.Monadic (assert)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), (@?), testCase)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Monadic as QC

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (Key, fromText)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as KeyMap
#endif


-- | Run this spec.
main :: IO ()
main = defaultMain tests

-- | Aeson tests
tests :: TestTree
tests = testGroup "hslua-aeson"
  [ testGroup "pushNull"
    [ testCase "pushes a value that is recognized as null when peeked" $ do
        val <- run @Lua.Exception (pushNull *> forcePeek (peekValue top))
        val @?= Aeson.Null
    , testCase "pushes a non-nil value" $ do
        nil <- run @Lua.Exception (pushNull *> isnil top)
        not nil @? "not of type `nil`"
    , testCase "pushes a single value" $ do
        diff <- run $ stackDiff pushNull
        diff @?= 1
    , testCase "pushes two values when called twice" $ do
        diff <- run $ stackDiff (pushNull *> pushNull)
        diff @?= 2
    ]
  , testGroup "Value"
    [ testProperty "can be round-tripped through the stack" $
      assertRoundtripEqual pushValue peekValue
    , testProperty "can roundtrip a bool nested in 50 layers of arrays" $
      \b -> QC.monadicIO $ do
        let go _ x = Aeson.Array $ Vector.fromList [x]
            mkValue a = foldr go (Aeson.Bool a) [ (1::Int) .. 50]
        x <- QC.run . run @Lua.Exception $ do
          pushValue $ mkValue b
          forcePeek $ peekValue top
        return (x === mkValue b)
    , testProperty "can roundtrip a bool nested in 50 layers of objects" $
      \b -> QC.monadicIO $ do
        let go _ x = Aeson.Object $ KeyMap.fromList [("x", x)]
            mkValue a = foldr go (Aeson.Bool a) [ (1::Int) .. 50]
        x <- QC.run . run @Lua.Exception $ do
          pushValue $ mkValue b
          forcePeek $ peekValue top
        return (x === mkValue b)
    ]

  , testGroup "Value component"
    [ testGroup "Scientific"
      [ testProperty "is converted to a Lua number" $ \x ->
          luaTest "type(x) == 'number'" ("x", x, pushScientific)
      , testProperty "double precision numbers can be round-tripped" $ \x ->
          assertRoundtripEqual pushScientific peekScientific
                               (luaNumberToScientific (Lua.Number x))
      , testProperty "can be round-tripped and stays approximately equal"
          assertRoundtripApprox
      ]
    , testGroup "Vector"
      [ testProperty "is converted to a Lua table" $ \x ->
          luaTest "type(x) == 'table'" ("x", x, pushVector pushBool)
      , testProperty "can contain Bools and be round-tripped"  $
          assertRoundtripEqual (pushVector pushBool) (peekVector peekBool)
      , testProperty "can contain Text and be round-tripped" $
          assertRoundtripEqual (pushVector pushText) (peekVector peekText)
      , testProperty "can contain Aeson.Value and be round-tripped" $
          assertRoundtripEqual (pushVector pushValue)
                               (peekVector peekValue)
      ]
    , testGroup "KeyMap"
      [ testProperty "is converted to a Lua table"  $ \x ->
        luaTest "type(x) == 'table'" ("x", x, pushKeyMap pushText)
      , testProperty "can be round-tripped with Bool values" $
          assertRoundtripEqual (pushKeyMap pushBool)
                               (peekKeyMap peekBool)
          . KeyMap.fromList
      , testProperty "can be round-tripped with Text values" $
          assertRoundtripEqual (pushKeyMap pushText)
                               (peekKeyMap peekText)
          . KeyMap.fromList
      , testProperty "can be round-tripped with Aeson.Value values"  $
          assertRoundtripEqual (pushKeyMap pushValue)
                               (peekKeyMap peekValue)
          . KeyMap.fromList
      ]
    ]
  ]

assertRoundtripApprox :: Scientific -> Property
assertRoundtripApprox x = QC.monadicIO $ do
  y <- QC.run $
       roundtrip (pushScientific @Lua.Exception)
                 (peekScientific @Lua.Exception)
                 x
  let xdouble = toRealFloat x :: Double
  let ydouble = toRealFloat y :: Double
  assert (xdouble ~== ydouble)

assertRoundtripEqual :: Eq a
                     => Pusher Lua.Exception a -> Peeker Lua.Exception a
                     -> a -> Property
assertRoundtripEqual pushX peekX x = QC.monadicIO $ do
  y <- QC.run $ roundtrip pushX peekX x
  assert (x == y)

roundtrip :: Pusher Lua.Exception a -> Peeker Lua.Exception a -> a -> IO a
roundtrip pushX peekX x = run $ do
  pushX x
  size <- gettop
  when (size /= 1) $
    failLua $ "not the right amount of elements on the stack: " ++ show size
  forcePeek $ peekX top

stackDiff :: Lua a -> Lua StackIndex
stackDiff op = do
  topBefore <- gettop
  _ <- op
  topAfter <- gettop
  return (topAfter - topBefore)

luaTest :: ByteString -> (Name, a, Pusher Lua.Exception a) -> Property
luaTest luaProperty (var, val, pushVal) = QC.monadicIO $ do
  result <- QC.run . run $ do
    openlibs
    pushVal val *> setglobal var
    _ <- dostring $ "return (" <> luaProperty <> ")"
    toboolean top
  assert result

luaNumberToScientific :: Lua.Number -> Scientific
luaNumberToScientific = fromFloatDigits . (realToFrac :: Lua.Number -> Double)

instance Arbitrary Aeson.Value where
  arbitrary = arbitraryValue 5

#if MIN_VERSION_aeson(2,0,0)
instance Arbitrary Key where
  arbitrary = fmap fromText arbitrary

instance Arbitrary a => Arbitrary (KeyMap.KeyMap a) where
  arbitrary = fmap KeyMap.fromList arbitrary
#endif

arbitraryValue :: Int -> Gen Aeson.Value
arbitraryValue size = frequency
    [ (1, return Aeson.Null)
    , (4, Aeson.Bool <$> arbitrary)
    -- Note: we don't draw numbers from the whole possible range, but
    -- only from the range of numbers that Lua can handle without
    -- rounding errors. This is ok, as JSON doesn't define a required
    -- precision, and (usually) matches the behavior of JavaScript.
    , (4, Aeson.Number . luaNumberToScientific . Lua.Number <$> arbitrary)
    , (4, Aeson.String <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Array <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Object <$> arbitrary)
    ]
