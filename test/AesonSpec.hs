{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   :  © 2017–2018 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (forM_, when)
import Data.AEq ((~==))
import Data.HashMap.Lazy (HashMap)
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Vector (Vector)
import Foreign.Lua as Lua
import Foreign.Lua.Aeson (pushNull)
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "pushNull" $ do
    it "pushes a value that is recognized as null when peeked" $ do
      val <- run (pushNull *> peek stackTop)
      assert (val == Aeson.Null)
    it "pushes a non-nil value" $ do
      nil <- run (pushNull *> isnil stackTop)
      assert (not nil)
    it "pushes a single value" $ do
      diff <- run $ stackDiff pushNull
      assert (diff == 1)
    it "pushes two values when called twice" $ do
      diff <- run $ stackDiff (pushNull *> pushNull)
      assert (diff == 2)
  describe "Value component" $ do
    describe "Scientific" $ do
      it "can be converted to a lua number" $ property $
        \x -> assert =<< luaTest "type(x) == 'number'" [("x", x::Scientific)]
      it "can be round-tripped through the stack with numbers of double precision" $
        property $ \x -> assertRoundtripEqual (luaNumberToScientific (Lua.Number x))
      it "can be round-tripped through the stack and stays approximately equal" $
        property $ \x -> assertRoundtripApprox (x :: Scientific)
    describe "Text" $ do
      it "can be converted to a lua string" $ property $
        \x -> assert =<< luaTest "type(x) == 'string'" [("x", x::Text)]
      it "can be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Text)
    describe "Vector" $ do
      it "is converted to a lua table" $ property $
        \x -> assert =<< luaTest "type(x) == 'table'" [("x", x::Vector Bool)]
      it "can contain Bools and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Vector Bool)
      it "can contain Texts and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::Vector Text)
      it "can contain Vector of Bools and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::(Vector (Vector Bool)))
    describe "HashMap" $ do
      it "is converted to a lua table" $ property $
        \x -> assert =<< luaTest "type(x) == 'table'" [("x", x::HashMap Text Bool)]
      it "can be round-tripped through the stack with Text keys and Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text Bool)
      it "can be round-tripped through the stack with Text keys and Vector Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text (Vector Bool))
    describe "Value" $
      it "can be round-tripped through the stack" . property $
        \x -> assertRoundtripEqual (x::Aeson.Value)

assertRoundtripApprox :: Scientific -> IO ()
assertRoundtripApprox x = do
  y <- roundtrip x
  let xdouble = toRealFloat x :: Double
  let ydouble = toRealFloat y :: Double
  assert (xdouble ~== ydouble)

assertRoundtripEqual :: (Show a, Eq a, Pushable a, Peekable a) => a -> IO ()
assertRoundtripEqual x = do
  y <- roundtrip x
  assert (x == y)

roundtrip :: (Pushable a, Peekable a) => a -> IO a
roundtrip x = run $ do
  push x
  size <- gettop
  when (size /= 1) $
    Prelude.error ("not the right amount of elements on the stack: " ++ show size)
  peek (-1)

stackDiff :: Lua a -> Lua StackIndex
stackDiff op = do
  topBefore <- gettop
  _ <- op
  topAfter <- gettop
  return (topAfter - topBefore)

luaTest :: Pushable a => String -> [(String, a)] -> IO Bool
luaTest luaTestCode xs = run $ do
  openlibs
  forM_ xs $ \(var, value) ->
    push value *> setglobal var
  let luaScript = "function run() return (" ++ luaTestCode ++ ") end"
  _ <- dostring (Char8.pack luaScript)
  callFunc "run"

luaNumberToScientific :: Lua.Number -> Scientific
luaNumberToScientific = fromFloatDigits . (realToFrac :: Lua.Number -> Double)

instance Arbitrary Aeson.Value where
  arbitrary = arbitraryValue 7

arbitraryValue :: Int -> Gen Aeson.Value
arbitraryValue size = frequency
    [ (1, return Aeson.Null)
    , (4, Aeson.Bool <$> arbitrary)
    -- FIXME: this is cheating: we don't draw numbers from the whole possible
    -- range, but only fro the range of nubers that can pass through the lua
    -- stack without rounding errors. Good enough for now, but we still might
    -- want to do better in the future.
    , (4, Aeson.Number . luaNumberToScientific . Lua.Number <$> arbitrary)
    , (4, Aeson.String <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Array <$> arbitrary)
    , (2, resize (size - 1) $ Aeson.Object <$> arbitrary)
    ]
