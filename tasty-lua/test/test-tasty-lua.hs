{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the @tasty@ Lua module.
-}

import Control.Monad (void)
import HsLua (Lua)
import System.Directory (withCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (pushModule, testLuaFile, translateResultsFromFile)

import qualified HsLua as Lua

main :: IO ()
main = do
  luaTest <- withCurrentDirectory "test" . Lua.run $
    translateResultsFromFile "test-tasty.lua"
  defaultMain $ testGroup "tasty-hslua" [luaTest, tests]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua tasty module"
  [ testCase "can be pushed to the stack" $
      Lua.run (void pushModule :: Lua ())

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

  , testGroup "testFileWith"
    [ testLuaFile Lua.run "test-tasty.lua" ("test" </> "test-tasty.lua")
    ]
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
