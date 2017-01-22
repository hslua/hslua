{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   :  © 2017 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (forM_)
import Data.Char (toLower)
import Scripting.Lua.Aeson (StackValue)
import qualified Data.Scientific as Sci
import qualified Scripting.Lua as Lua

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | Run this spec.
main :: IO ()
main = hspec spec

-- | Specifications for Attributes parsing functions.
spec :: Spec
spec = do
  describe "Value component" $ do
    describe "Boolean" $ do
      it "can be converted to a lua boolean" $ property $
        \x -> assert =<< luaTest "type(x) == 'boolean'" [("x", x::Bool)]
      it "can be read from a lua boolean on the stack" $ property $
        \x -> assert =<< luaReturnEqual (map toLower $ show (x::Bool)) x
    describe "Scientific" $ do
      it "can be converted to a lua number" $ property $
        \x -> assert =<< luaTest "type(x) == 'number'" [("x", x::Sci.Scientific)]
      it "can be read from a lua number on the stack" $ property $
        \x -> assert =<< luaReturnApprox (show x) x

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

luaReturnApprox :: String -> Sci.Scientific -> IO Bool
luaReturnApprox valStr val = do
  retVal <- Sci.toRealFloat <$> luaReturn valStr
  let doubleVal = (Sci.toRealFloat val)::Double
  return $ doubleVal == retVal

luaReturnEqual :: (Eq a, StackValue a) => String -> a -> IO Bool
luaReturnEqual valStr val = do
  retVal <- luaReturn valStr
  return $ val == retVal

luaReturn :: StackValue a => String -> IO a
luaReturn luaValue = do
  lua <- Lua.newstate
  let luaScript = "function run() return (" ++ luaValue ++ ") end"
  Lua.openlibs lua
  _ <- Lua.loadstring lua luaScript "test script"
  Lua.call lua 0 0
  retval <- Lua.callfunc lua "run"
  Lua.close lua
  return retval
