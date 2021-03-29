{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : Main
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : stable
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `text` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import HsLua.Core as Lua
import HsLua.Packaging
import HsLua.Module.Text (documentedModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

main :: IO ()
main = do
  luaTest <- run @Lua.Exception $ do
    openlibs
    registerModule documentedModule
    pop 1
    translateResultsFromFile "test/test-text.lua"
  defaultMain $ testGroup "hslua-module-text" [tests, luaTest]

-- | HSpec tests
tests :: TestTree
tests = testGroup "FromLuaStack"
  [ testCase "text module can be pushed to the stack" $
      run (void (pushModule documentedModule) :: Lua ())

  , testCase "text module can be added to the preloader" . run $ do
      openlibs
      preloadModule documentedModule
      assertEqual' "function not added to preloader" TypeFunction =<< do
        _ <- dostring "return package.preload.text"
        ltype top

  , testCase "text module can be loaded as hstext" . run $ do
      openlibs
      preloadModuleWithName documentedModule "hstext"
      assertEqual' "loading the module fails " OK =<<
        dostring "require 'hstext'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = liftIO . assertEqual msg expected
