{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-|
Copyright   :  © 2017–2021 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (when)
import Data.AEq ((~==))
import Data.ByteString (ByteString)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import HsLua as Lua hiding (Property, property)
import HsLua.Aeson
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Vector as Vector

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "pushNull" $ do
    it "pushes a value that is recognized as null when peeked" $ do
      val <- run @Lua.Exception (pushNull *> forcePeek (peekValue top))
      assert (val == Aeson.Null)
    it "pushes a non-nil value" $ do
      nil <- run @Lua.Exception (pushNull *> isnil top)
      assert (not nil)
    it "pushes a single value" $ do
      diff <- run $ stackDiff pushNull
      assert (diff == 1)
    it "pushes two values when called twice" $ do
      diff <- run $ stackDiff (pushNull *> pushNull)
      assert (diff == 2)
  describe "Value" $ do
    it "can be round-tripped through the stack" . property $
      assertRoundtripEqual pushValue peekValue
    it "can roundtrip a bool nested in 50 layers of arrays" $
      property $ \b -> do
        let go _ x = Aeson.Array $ Vector.fromList [x]
            mkValue a = foldr go (Aeson.Bool a) [ (1::Int) .. 50]
        x <- run @Lua.Exception $ do
          pushValue $ mkValue b
          size <- gettop
          when (size /= 1) $
            failLua $ "elements on stack (should be 1): " ++ show size
          forcePeek $! peekValue top
        assert $! (x == mkValue b)
    it "can roundtrip a bool nested in 50 layers of objects" $
      property $ \b -> do
        let go _ x = Aeson.Object $ HashMap.fromList [("x", x)]
            mkValue a = foldr go (Aeson.Bool a) [ (1::Int) .. 50]
        x <- run @Lua.Exception $ do
          pushValue $ mkValue b
          size <- gettop
          when (size /= 1) $
            failLua $ "elements on stack (should be 1): " ++ show size
          forcePeek $! peekValue top
        assert $! (x == mkValue b)

  describe "Value component" $ do
    describe "Scientific" $ do
      it "is converted to a Lua number" $ property $ \x -> assert =<<
        luaTest "type(x) == 'number'" ("x", x, pushScientific)
      it "double precision numbers can be round-tripped" $
        property $ \x ->
        assertRoundtripEqual pushScientific peekScientific
                             (luaNumberToScientific (Lua.Number x))
      it "can be round-tripped and stays approximately equal" $
        property assertRoundtripApprox
    describe "Vector" $ do
      it "is converted to a Lua table" $ property $ \x -> assert =<<
        luaTest "type(x) == 'table'" ("x", x, pushVector pushBool)
      it "can contain Bools and be round-tripped" $ property $
        assertRoundtripEqual (pushVector pushBool) (peekVector peekBool)
      it "can contain Text and be round-tripped" $ property $
        assertRoundtripEqual (pushVector pushText) (peekVector peekText)
      it "can contain Aeson.Value and be round-tripped" $ property $
        assertRoundtripEqual (pushVector pushValue)
                             (peekVector peekValue)
    describe "HashMap" $ do
      it "is converted to a Lua table" $ property $ \x -> assert =<<
        luaTest "type(x) == 'table'" ("x", x, pushHashMap pushText pushText)
      it "can be round-tripped with Bool values" $ property $
        assertRoundtripEqual (pushHashMap pushText pushBool)
                             (peekHashMap peekText peekBool)
        . HashMap.fromList
      it "can be round-tripped with Text values" $ property $
        assertRoundtripEqual (pushHashMap pushText pushText)
                             (peekHashMap peekText peekText)
        . HashMap.fromList
      it "can be round-tripped with Aeson.Value values" $ property $
        assertRoundtripEqual (pushHashMap pushText pushValue)
                             (peekHashMap peekText peekValue)
        . HashMap.fromList

assertRoundtripApprox :: Scientific -> IO ()
assertRoundtripApprox x = do
  y <- roundtrip (pushScientific @Lua.Exception)
                 (peekScientific @Lua.Exception)
                 x
  let xdouble = toRealFloat x :: Double
  let ydouble = toRealFloat y :: Double
  assert (xdouble ~== ydouble)

assertRoundtripEqual :: (Show a, Eq a)
                     => Pusher Lua.Exception a -> Peeker Lua.Exception a
                     -> a -> IO ()
assertRoundtripEqual pushX peekX x = do
  y <- roundtrip pushX peekX x
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

luaTest :: ByteString -> (Name, a, Pusher Lua.Exception a) -> IO Bool
luaTest luaProperty (var, val, pushVal) = run $ do
  openlibs
  pushVal val *> setglobal var
  _ <- dostring $ "return (" <> luaProperty <> ")"
  toboolean top

luaNumberToScientific :: Lua.Number -> Scientific
luaNumberToScientific = fromFloatDigits . (realToFrac :: Lua.Number -> Double)

instance Arbitrary Aeson.Value where
  arbitrary = arbitraryValue 5

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
