{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Test.Tasty.Lua.Core
Copyright   : © 2019–2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Core types and functions for tasty Lua tests.
-}
module Test.Tasty.Lua.Core
  ( runTastyFile
  , ResultTree (..)
  , Outcome (..)
  , UnnamedTree (..)
  )
where

import Control.Monad ((<$!>), void)
import Data.ByteString (ByteString)
import HsLua.Core (LuaE, LuaError, absindex, pop, toboolean, top)
import HsLua.Marshalling
  ( Peeker, failPeek, liftLua, resultToEither, retrieving
  , peekList, peekString, runPeek, typeMismatchMessage)
import Test.Tasty.Lua.Module (pushModule)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import qualified HsLua.Core as Lua
import qualified Test.Tasty as Tasty

-- | Run a tasty Lua script from a file and return either the resulting
-- test tree or the error message.
runTastyFile :: LuaError e => FilePath -> LuaE e (Either String [ResultTree])
runTastyFile fp = do
  Lua.openlibs
  Lua.requirehs "tasty" (const . void $ pushModule)
  res <- Lua.dofile fp
  if res /= Lua.OK
    then Left . toString <$> Lua.tostring' top
    else resultToEither <$> runPeek (peekList peekResultTree top)

-- | Convert UTF8-encoded @'ByteString'@ to a @'String'@.
toString :: ByteString -> String
toString = T.unpack . T.Encoding.decodeUtf8


-- | Tree of test results returned by tasty Lua scripts. This is
-- similar to tasty's @'TestTree'@, with the important difference that
-- all tests have already been run, and all test results are known.
data ResultTree = ResultTree Tasty.TestName UnnamedTree

peekResultTree :: LuaError e => Peeker e ResultTree
peekResultTree idx = do
  idx'   <- liftLua $ absindex idx
  name   <- liftLua (Lua.getfield idx' "name")   *> peekString top
  result <- liftLua (Lua.getfield idx' "result") *> peekUnnamedTree top
  liftLua $ pop 2
  return $! ResultTree name result


-- | Either a raw test outcome, or a nested @'Tree'@.
data UnnamedTree
  = SingleTest Outcome
  | TestGroup [ResultTree]

-- | Unmarshal an @'UnnamedTree'@.
peekUnnamedTree :: LuaError e => Peeker e UnnamedTree
peekUnnamedTree idx = liftLua (Lua.ltype idx) >>= \case
  Lua.TypeTable -> TestGroup   <$!> peekList peekResultTree idx
  _             -> SingleTest  <$!> peekOutcome idx


-- | Test outcome
data Outcome = Success | Failure String

-- | Unmarshal a test outcome
peekOutcome :: LuaError e => Peeker e Outcome
peekOutcome idx = retrieving "test result" $ do
  liftLua (Lua.ltype idx) >>= \case
    Lua.TypeString  -> Failure <$!> peekString idx
    Lua.TypeBoolean -> do
      b <- liftLua $ toboolean idx
      return $ if b then Success else Failure "???"
    _ -> typeMismatchMessage "string or boolean" idx >>= failPeek
