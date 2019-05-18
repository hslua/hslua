{-# LANGUAGE LambdaCase #-}
{-|
Module      : Test.Tasty.Lua.Core
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : not portable, requires GHC or later

Core types and functions for tasty Lua tests.
-}
module Test.Tasty.Lua.Core
  ( runTastyFile
  , ResultTree (..)
  , Outcome (..)
  , UnnamedTree (..)
  )
where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Foreign.Lua (Lua, Peekable, StackIndex)
import Test.Tasty.Lua.Module (pushModule)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Foreign.Lua as Lua
import qualified Test.Tasty as Tasty

-- | Run a tasty Lua script from a file and return either the resulting
-- test tree or the error message.
runTastyFile :: FilePath -> Lua (Either String [ResultTree])
runTastyFile fp = do
  Lua.openlibs
  Lua.requirehs "tasty" (void pushModule)
  res <- Lua.dofile fp
  if res /= Lua.OK
    then Left . toString <$> Lua.tostring' Lua.stackTop
    else Lua.try (Lua.peekList Lua.stackTop) >>= \case
           Left (Lua.Exception e) -> return (Left e)
           Right trees            -> return (Right trees)

-- | Convert UTF8-encoded @'ByteString'@ to a @'String'@.
toString :: ByteString -> String
toString = Text.unpack . Text.Encoding.decodeUtf8

-- | Tree of test results returned by tasty Lua scripts. This is
-- similar to tasty's @'TestTree'@, with the important difference that
-- all tests have already been run, and all test results are known.
data ResultTree = ResultTree Tasty.TestName UnnamedTree

instance Peekable ResultTree where
  peek = peekResultTree

peekResultTree :: StackIndex -> Lua ResultTree
peekResultTree idx = do
  name   <- Lua.getfield idx "name"   *> Lua.popValue
  result <- Lua.getfield idx "result" *> Lua.popValue
  return $ ResultTree name result

-- | Either a raw test outcome, or a nested @'Tree'@.
data UnnamedTree
  = SingleTest Outcome
  | TestGroup [ResultTree]

instance Peekable UnnamedTree where
  peek = peekUnnamedTree

-- | Unmarshal an @'UnnamedTree'@.
peekUnnamedTree :: StackIndex -> Lua UnnamedTree
peekUnnamedTree idx = do
  ty <- Lua.ltype idx
  case ty of
    Lua.TypeTable   -> TestGroup   <$> Lua.peekList idx
    _               -> SingleTest  <$> Lua.peek idx


-- | Test outcome
data Outcome = Success | Failure String

instance Peekable Outcome where
  peek = peekOutcome

-- | Unmarshal a test outcome
peekOutcome :: StackIndex -> Lua Outcome
peekOutcome idx = do
  ty <- Lua.ltype idx
  case ty of
    Lua.TypeString  -> Failure <$> Lua.peek idx
    Lua.TypeBoolean -> do
      b <- Lua.peek idx
      return $ if b then Success else Failure "???"
    _ -> do
      s <- toString <$> Lua.tostring' idx
      Lua.throwException ("not a test result: " ++ s)
