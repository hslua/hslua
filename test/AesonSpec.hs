{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   :  © 2017 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (forM_)
import Data.AEq ((~==))
import Data.HashMap.Lazy (HashMap)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import Data.Vector (Vector)
import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Scripting.Lua.Aeson (StackValue)

import qualified Scripting.Lua as Lua

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "Value component" $ do
    describe "Scientific" $ do
      it "can be converted to a lua number" $ property $
        \x -> assert =<< luaTest "type(x) == 'number'" [("x", x::Scientific)]
      it "can be round-tripped through the stack (lossy)" $ property $
        \x -> assertRoundtripApprox (x::Scientific)
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
      it "can contain Bools and be round-tripped through the stack" $ property $
        \x -> assertRoundtripEqual (x::(Vector (Vector Bool)))
    describe "HashMap" $ do
      it "is converted to a lua table" $ property $
        \x -> assert =<< luaTest "type(x) == 'table'" [("x", x::HashMap Text Bool)]
      it "can be round-tripped through the stack with Text keys and Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text Bool)
      it "can be round-tripped through the stack with Text keys and Vector Bool values" $
        property $ \x -> assertRoundtripEqual (x::HashMap Text (Vector Bool))

assertRoundtripApprox :: Scientific -> IO ()
assertRoundtripApprox x = do
  y <- roundtrip x
  let xdouble = toRealFloat x :: Double
  let ydouble = toRealFloat y :: Double
  assert (xdouble ~== ydouble)

assertRoundtripEqual :: (Show a, Eq a, StackValue a) => a -> IO ()
assertRoundtripEqual x = do
  y <- roundtrip x
  assert (x == y)

roundtrip :: (StackValue a) => a -> IO a
roundtrip x = do
  lua <- Lua.newstate
  Lua.push lua x
  res <- Lua.peek lua (-1)
  retval <- res `seq` case res of
        Nothing -> error "could not read from stack"
        Just y  -> do
          return y
  Lua.close lua
  return retval

luaTest :: StackValue a => String -> [(String, a)] -> IO Bool
luaTest luaTestCode xs = do
  lua <- Lua.newstate
  forM_ xs $ \(var, value) ->
    Lua.push lua value *> Lua.setglobal lua var
  let luaScript = "function run() return (" ++ luaTestCode ++ ") end"
  Lua.openlibs lua
  _ <- Lua.loadstring lua luaScript "test script"
  Lua.call lua 0 0
  retval <- Lua.callfunc lua "run"
  Lua.close lua
  return retval
