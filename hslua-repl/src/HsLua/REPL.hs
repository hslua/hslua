{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : HsLua.REPL
Copyright   : Copyright © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Embeddable Lua interpreter interface.
-}
module HsLua.REPL
  ( -- * Run scripts as program
    repl
  , replWithEnv
  , setup
  , Config (..)
  , defaultConfig
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Lua (pattern LUA_COPYRIGHT)
import HsLua.Core
import System.Console.Isocline
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified HsLua.Core.Utf8 as UTF8

-- | Lua runner command line options.
data Config = Config
  { replPrompt     :: Text
  , replInfo       :: Text
  , replHistory    :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config
  { replPrompt = ""
  , replInfo = T.pack LUA_COPYRIGHT
  , replHistory = Nothing
  }

-- | Setup a new repl. Prints the version and extra info before the
-- first prompt.
setup :: Config -> LuaE e ()
setup config = do
  liftIO $ T.putStrLn (replInfo config)
  case replHistory config of
    Just histfile -> liftIO $ setHistory histfile 200
    Nothing -> pure ()

-- | Checks if the error message hints at incomplete input. Removes the
-- message from the stack in that case.
incomplete :: LuaError e => LuaE e Bool
incomplete = do
  let eofmark = "<eof>"
  msg <- tostring' top
  if eofmark `Char8.isSuffixOf` msg
    then True  <$ pop 2  -- error message (duplicated by tostring')
    else False <$ pop 1  -- value pushed by tostring'

-- | Load an input string, mark it as coming from @stdin@.
loadinput :: ByteString -> LuaE e Status
loadinput inp = loadbuffer inp "=stdin"

-- | Try to load input while prepending a @return@ statement.
loadExpression :: LuaError e => ByteString -> LuaE e ()
loadExpression input = loadinput ("return " <> input) >>= \case
  OK -> pure ()  -- yep, that worked
  _err   -> throwErrorAsException

-- | Load a multiline statement; prompts for more lines if the statement
-- looks incomplete.
loadStatement :: LuaError e
              => [ByteString]      -- ^ input lines
              -> LuaE e ()
loadStatement lns = do
  loadinput (Char8.unlines $ reverse lns) >>= \case
    OK -> pure ()
    ErrSyntax -> incomplete >>= \isincmplt ->
      if isincmplt
      then liftIO (readlineMaybe ">") >>= \case
        Nothing    -> failLua "Multiline input aborted"
        Just input -> loadStatement (UTF8.fromString input : lns)
      else throwErrorAsException
    _ -> throwErrorAsException

-- | Run a Lua REPL.
repl :: LuaError e => LuaE e NumResults
repl = replWithEnv Nothing

-- | Run a Lua REPL, using the table in the given upvalue as the load
-- environment.
replWithEnv :: LuaError e => Maybe Reference -> LuaE e NumResults
replWithEnv mEnvRef = try repl' >>= \case
  Right n  -> pure n -- Ctrl-D or Ctrl-C
  Left err -> do
    -- something went wrong: report error, reset stack and try again
    void $ getglobal "print"
    pushException err
    call 1 0
    settop 0
    replWithEnv mEnvRef
 where
  repl' :: LuaError e => LuaE e NumResults
  repl' = liftIO (readlineMaybe "") >>= \case
    Nothing ->
      -- Return all values left on the stack as results
      NumResults . fromStackIndex <$> gettop
    Just inputStr -> do
      settop 0  -- reset stack
      let input = UTF8.fromString inputStr
      loadExpression input <|> loadStatement [input]
      -- take env (if any) and set it as the first upvalue of the loaded
      -- thunk.
      case mEnvRef of
        Nothing -> pure ()
        Just envRef -> do
          getref registryindex envRef >>= \case
            TypeTable -> void $ setupvalue (nth 2) 1
            _ -> pop 1
      -- run loaded input
      callTrace 0 multret
      nvalues <- gettop
      -- duplicate everything and call print on the results
      checkstack' (fromIntegral (fromStackIndex nvalues) + 1) "repl'"
      when (nvalues > 0) $ do
        void $ getglobal "print"
        mapM_ pushvalue [1..nvalues]
        call (fromIntegral $ fromStackIndex nvalues) 0
      repl'
