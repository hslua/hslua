{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Main where

import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Char                   (isSpace)
import           System.Directory            (getTemporaryDirectory, removeFile)
import           System.IO
import           System.Cmd                 (system)

import qualified Scripting.Lua               as Lua

import Debug.Trace


newtype Repl a = Repl { unwrapRepl :: StateT Lua.LuaState (WriterT [String] IO) a }
      deriving (Functor, Monad, MonadState Lua.LuaState, MonadWriter [String], MonadIO)

checkEdit :: String -> Bool
checkEdit = (==) ":edit" . take 5 . dropWhile isSpace

edit :: (MonadIO m, MonadWriter [String] m) => m String
edit = do
    tempDir <- liftIO getTemporaryDirectory
    (path, handle) <- liftIO $ openTempFile tempDir "temp.luarepl"
    rls <- liftM (reverse . snd) $ listen (return ())
    trace ("rls: " ++ (unlines rls)) (return ())
    contents <- liftIO $ do
      hPutStr handle (unlines rls)
      hFlush handle
      void $ system $ "vim " ++ path
      hSeek handle AbsoluteSeek 0
      contents <- liftIO $ hGetContents handle
      hClose handle
      removeFile path
      return contents
    return (trace contents contents)

readAndRun :: Repl ()
readAndRun = do
    l <- get
    liftIO $ do
      putStr "> "
      hFlush stdout
    line <- liftIO getLine
    if checkEdit line
      then do
        contents <- edit
        l' <- liftIO $ do
          Lua.close l
          l' <- Lua.newstate
          Lua.openlibs l'

          Lua.loadstring l' contents contents
          Lua.pcall l' 0 0 0

          return l'
        put l'
      else do
        void $ liftIO $ Lua.loadstring l line line
        retCode <- liftIO $ Lua.pcall l 0 0 0
        case retCode of
          Lua.PCOK     -> trace ("tell: " ++ line) (tell [line])
          Lua.PCERRRUN -> liftIO $ putStrLn "ERRRUN"
          Lua.PCERRMEM -> liftIO $ putStrLn "ERRMEM"
          Lua.PCERRERR -> liftIO $ putStrLn "ERRERR"
    readAndRun

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    putStrLn ("Lua version: " ++ show Lua.lua_version_num)
    void $ runWriterT (runStateT (unwrapRepl readAndRun) l)
    Lua.close l
