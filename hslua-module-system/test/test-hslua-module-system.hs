{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : Main
Copyright   : © 2019-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `system` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua.Core as Lua
import HsLua.Module.System (documentedModule)
import HsLua.Packaging.Module
  (preloadModule, preloadModuleWithName, pushModule, registerModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  luaTestResults <- run @Lua.Exception $ do
    openlibs
    registerModule documentedModule
    pop 1
    translateResultsFromFile "test/test-system.lua"
  defaultMain $ testGroup "hslua-module-system" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua System module"
  [ testCase "system module can be pushed to the stack" $
      run (void (pushModule documentedModule) :: Lua ())

  , testCase "system module can be added to the preloader" . run $ do
      openlibs
      preloadModule documentedModule
      assertEqual' "function not added to preloader" TypeFunction =<< do
        _ <- dostring "return package.preload.system"
        ltype top

  , testCase "system module can be loaded as hssystem" . run $ do
      openlibs
      preloadModuleWithName documentedModule "hssystem"
      assertEqual' "loading the module fails " OK =<<
        dostring "require 'hssystem'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = liftIO . assertEqual msg expected
