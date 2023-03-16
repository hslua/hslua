{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
Module      : Main
Copyright   : © 2022-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

A simple Lua REPL.
-}
module Main (main) where
import Control.Monad (void)
import HsLua.Core  as Lua (Exception, openlibs, run)
import HsLua.REPL (Config (..), defaultConfig, repl, setup)
import qualified Data.Text as T

-- | Run a default Lua interpreter.
main :: IO ()
main = run @Lua.Exception $ do
  let config = Config
        { replPrompt = "luarepl"
        , replInfo = replInfo defaultConfig `T.append`
                     "\nREPL: © 2023 Albert Krewinkel"
        , replHistory = Just ".luarepl-history"
        }
  openlibs
  setup config
  void repl
