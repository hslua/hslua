{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
Module      : Main
Copyright   : © 2022-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Re-implementation of the standard Lua interpreter.
-}
module Main (main) where
import Control.Monad (when)
import HsLua.Core  as Lua
  (Exception, openlibs, pushboolean, registryindex, run, setfield)
import HsLua.CLI (EnvBehavior (IgnoreEnvVars), Settings (..), runStandalone)
import System.Environment (getArgs, getProgName)

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
            action
        , settingsHistory = Just ".hslua-history"
        }
  prg  <- getProgName
  args <- getArgs
  runStandalone @Lua.Exception settings prg args
