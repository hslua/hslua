{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : Main
Copyright   : © 2021-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : stable
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `zip` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua.Core (Lua, top)
import HsLua.Packaging ( preloadModule, preloadModuleWithName, pushModule
                       , registerModule)
import HsLua.Module.Zip (documentedModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified HsLua.Core as Lua
import qualified HsLua.Module.System as System

main :: IO ()
main = do
  luaTestResults <- Lua.run @Lua.Exception $ do
    Lua.openlibs
    registerModule documentedModule
    registerModule System.documentedModule
    Lua.pop 1
    translateResultsFromFile "test/test-zip.lua"
  defaultMain $ testGroup "hslua-module-zip" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua zip module"
  [ testCase "zip module can be pushed to the stack" $
      Lua.run (void (pushModule documentedModule) :: Lua ())

  , testCase "zip module can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      preloadModule documentedModule
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        void $ Lua.getglobal "package"
          *> Lua.getfield top "preload"
          *> Lua.getfield top "zip"
        Lua.ltype (-1)

  , testCase "zip module can be loaded as hszip" . Lua.run $ do
      Lua.openlibs
      preloadModuleWithName documentedModule "hszip"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'hszip'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
