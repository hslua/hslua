{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
Module      : Main
Copyright   : © 2022-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

A Haskell-wrapped Lua interpreter with Haskell modules.
-}
module Main (main) where
import Control.Monad (when)
import HsLua.Core  as Lua
  (Exception, openlibs, pushboolean, registryindex, run, setfield)
import HsLua.Packaging (preloadModule)
import HsLua.CLI (EnvBehavior (IgnoreEnvVars), Settings (..), runStandalone)
import System.Environment (getArgs, getProgName)
import qualified HsLua.Module.Path    as Path
import qualified HsLua.Module.System  as System
import qualified HsLua.Module.Text    as Text
import qualified HsLua.Module.Version as Version
import qualified HsLua.Module.Zip     as Zip

-- | Run a default Lua interpreter.
main :: IO ()
main = do
  let settings = Settings
        { settingsVersionInfo = ""
        , settingsRunner = \envBehavior action -> run $ do
            when (envBehavior == IgnoreEnvVars) $ do
              pushboolean True
              setfield registryindex "LUA_NOENV"
            openlibs
            mapM_ preloadModule
              [ Path.documentedModule
              , System.documentedModule
              , Text.documentedModule
              , Version.documentedModule
              , Zip.documentedModule
              ]
            action
        , settingsHistory = Just ".hslua-history"
        }
  prg  <- getProgName
  args <- getArgs
  runStandalone @Lua.Exception settings prg args
