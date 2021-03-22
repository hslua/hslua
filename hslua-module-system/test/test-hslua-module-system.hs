{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `system` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua.Core
import HsLua.Module.System (preloadModule, pushModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified HsLua.Core as Lua

main :: IO ()
main = do
  luaTestResults <- Lua.run $ do
    Lua.openlibs
    Lua.requirehs "system" (void pushModule)
    translateResultsFromFile "test/test-system.lua"
  defaultMain $ testGroup "hslua-module-system" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua System module"
  [ testCase "system module can be pushed to the stack" $
      Lua.run (void pushModule)

  , testCase "system module can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      preloadModule "system"
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        _ <- Lua.dostring "return package.preload.system"
        Lua.ltype top

  , testCase "system module can be loaded as hssystem" . Lua.run $ do
      Lua.openlibs
      preloadModule "hssystem"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'hssystem'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
