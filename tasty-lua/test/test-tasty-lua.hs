{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Main
Copyright   : © 2019-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for the @tasty@ Lua module.
-}

import Control.Monad (void)
import HsLua.Core (Lua)
import Lua.Arbitrary ()
import System.Directory (withCurrentDirectory)
import System.FilePath ((</>))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua
  (pushModule, registerArbitrary, testLuaFile, translateResultsFromFile)

import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua

main :: IO ()
main = do
  luaTest <- withCurrentDirectory "test" . Lua.run @Lua.Exception $ do
    registerCustom
    translateResultsFromFile "test-tasty.lua"
  defaultMain $ testGroup "tasty-hslua" [luaTest, tests]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua tasty module"
  [ testCase "can be pushed to the stack" . Lua.run $ do
      Lua.openlibs
      void pushModule :: Lua ()

  , testCase "can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      Lua.preloadhs "tasty" pushModule
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        Lua.loadstring "return package.preload.tasty" *> Lua.call 0 1
        Lua.ltype (-1)

  , testCase "can be loaded as tasty" . Lua.run $ do
      Lua.openlibs
      Lua.requirehs "tasty" (const $ void pushModule)
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'tasty'"

  , testGroup "testFileWith"
    [ testLuaFile
      (\x -> Lua.run @Lua.Exception $ do
        registerCustom
        x)
      "test-tasty.lua" ("test" </> "test-tasty.lua")
    ]
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected

registerCustom :: Lua ()
registerCustom = do
  registerArbitrary "custom" pushCustom nopeek
  registerArbitrary @[Integer] "integer_list"
    (Lua.pushList Lua.pushIntegral) (Lua.peekList Lua.peekIntegral)

-- | Custom type used for to check property testing.
newtype Custom = Custom Lua.Integer

instance Arbitrary Custom where
  arbitrary = Custom <$> arbitrary

pushCustom :: Lua.LuaError e => Lua.Pusher e Custom
pushCustom (Custom i) = do
  Lua.newtable
  Lua.pushName "int"
  Lua.pushinteger i
  Lua.rawset (Lua.nth 3)

nopeek :: Lua.Peeker e a
nopeek = const $ Lua.failPeek "nope"  -- do not allow peeking
