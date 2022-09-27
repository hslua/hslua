{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
Module      : Main
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

Re-implementation of the standard Lua interpreter.
-}
module Main (main) where
import Control.Monad (when)
import HsLua.Core  as Lua
  (Exception, openlibs, pushboolean, registryindex, run, setfield)
import HsLua.CLI (EnvVarOpt (IgnoreEnvVars), Settings (..), runStandalone)
import System.Environment (getArgs, getProgName)

-- | Run a default Lua interpreter.
main :: IO ()
main = do
  let settings = Settings
        { settingsVersionInfo = ""
        , settingsRunner = \envVarOpt action -> run $ do
            when (envVarOpt == IgnoreEnvVars) $ do
              pushboolean True
              setfield registryindex "LUA_NOENV"
            openlibs
            action
        }
  prg  <- getProgName
  args <- getArgs
  runStandalone @Lua.Exception settings prg args
