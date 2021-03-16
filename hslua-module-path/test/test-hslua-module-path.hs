{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : stable
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `path` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua (Lua)
import HsLua.Module.Path (preloadModule, pushModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified HsLua as Lua

main :: IO ()
main = do
  luaTestResults <- Lua.run $ do
    Lua.openlibs
    Lua.requirehs "path" (void pushModule)
    translateResultsFromFile "test/test-path.lua"
  defaultMain $ testGroup "hslua-module-path" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua path module"
  [ testCase "path module can be pushed to the stack" $
      Lua.run (void pushModule :: Lua ())

  , testCase "path module can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      preloadModule "path"
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        Lua.getglobal' "package.preload.path"
        Lua.ltype (-1)

  , testCase "path module can be loaded as hspath" . Lua.run $ do
      Lua.openlibs
      preloadModule "hspath"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'hspath'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
