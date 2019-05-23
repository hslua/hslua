{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `system` Lua module.
-}

import Control.Monad (void)
import Foreign.Lua (Lua)
import Foreign.Lua.Module.System (preloadModule, pushModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified Foreign.Lua as Lua

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
        Lua.getglobal' "package.preload.system"
        Lua.ltype (-1)

  , testCase "system module can be loaded as hssystem" . Lua.run $ do
      Lua.openlibs
      preloadModule "hssystem"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'hssystem'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
