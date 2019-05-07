{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the @tasty@ Lua module.
-}

import Control.Monad (void)
import Foreign.Lua (Lua)
import Foreign.Lua.Module.Tasty (pushModule, testsFromFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Foreign.Lua as Lua

main :: IO ()
main = do
  luaTest <- Lua.run $ testsFromFile "test/tasty-module-tests.lua"
  defaultMain $ testGroup "hslua-module-system" [luaTest, tests]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua tasty module"
  [ testCase "can be pushed to the stack" $
      Lua.run (void pushModule)

  , testCase "can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      Lua.preloadhs "tasty" pushModule
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        Lua.getglobal' "package.preload.tasty"
        Lua.ltype (-1)

  , testCase "can be loaded as tasty" . Lua.run $ do
      Lua.openlibs
      Lua.requirehs "tasty" (void pushModule)
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'tasty'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
