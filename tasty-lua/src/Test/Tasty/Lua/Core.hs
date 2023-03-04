{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Test.Tasty.Lua.Core
Copyright   : © 2019-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

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
import HsLua.Core (LuaE, LuaError, toboolean, top)
import HsLua.Marshalling
  ( Peeker, failPeek, liftLua, resultToEither, retrieving
  , peekFieldRaw, peekList, peekString, runPeek, typeMismatchMessage)
import Test.Tasty.Lua.Module (pushModule)
import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8
import qualified Test.Tasty as Tasty

-- | Run a tasty Lua script from a file and return either the resulting
-- test tree or the error message.
runTastyFile :: LuaError e => FilePath -> LuaE e (Either String [ResultTree])
runTastyFile fp = do
  Lua.openlibs
  Lua.requirehs "tasty" (const . void $ pushModule)
  res <- Lua.dofileTrace (Just fp)
  if res /= Lua.OK
    then Left . Utf8.toString <$> Lua.tostring' top
    else resultToEither <$> runPeek (peekList peekResultTree top)

-- | Tree of test results returned by tasty Lua scripts. This is
-- similar to tasty's @'TestTree'@, with the important difference that
-- all tests have already been run, and all test results are known.
data ResultTree = ResultTree Tasty.TestName UnnamedTree

peekResultTree :: LuaError e => Peeker e ResultTree
peekResultTree idx = do
  name   <- peekFieldRaw peekString "name" idx
  result <- peekFieldRaw peekUnnamedTree "result" idx
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
peekOutcome :: Peeker e Outcome
peekOutcome idx = retrieving "test result" $ do
  liftLua (Lua.ltype idx) >>= \case
    Lua.TypeString  -> Failure <$!> peekString idx
    Lua.TypeBoolean -> do
      b <- liftLua $ toboolean idx
      return $ if b then Success else Failure "???"
    _ -> typeMismatchMessage "string or boolean" idx >>= failPeek
