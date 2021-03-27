{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Test.Tasty.Lua.Core
Copyright   : © 2019–2021 Albert Krewinkel
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
import HsLua.Core
import HsLua.Marshalling
  ( Peeker, failure, resultToEither, retrieving
  , peekList, peekString, typeMismatchMessage)
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
  Lua.requirehs "tasty" (void pushModule)
  res <- Lua.dofile fp
  if res /= Lua.OK
    then Left . toString <$> Lua.tostring' top
    else resultToEither <$> peekList peekResultTree top

-- | Convert UTF8-encoded @'ByteString'@ to a @'String'@.
toString :: ByteString -> String
toString = T.unpack . T.Encoding.decodeUtf8


-- | Tree of test results returned by tasty Lua scripts. This is
-- similar to tasty's @'TestTree'@, with the important difference that
-- all tests have already been run, and all test results are known.
data ResultTree = ResultTree Tasty.TestName UnnamedTree

peekResultTree :: LuaError e => Peeker e ResultTree
peekResultTree idx = do
  idx'   <- absindex idx
  name   <- Lua.getfield idx' "name"   *> peekString top
  result <- Lua.getfield idx' "result" *> peekUnnamedTree top
  pop 2
  return $! ResultTree <$> name <*> result


-- | Either a raw test outcome, or a nested @'Tree'@.
data UnnamedTree
  = SingleTest Outcome
  | TestGroup [ResultTree]

-- | Unmarshal an @'UnnamedTree'@.
peekUnnamedTree :: LuaError e => Peeker e UnnamedTree
peekUnnamedTree idx = Lua.ltype idx >>= \case
  Lua.TypeTable -> fmap TestGroup   <$> peekList peekResultTree idx
  _             -> fmap SingleTest  <$> peekOutcome idx


-- | Test outcome
data Outcome = Success | Failure String

-- | Unmarshal a test outcome
peekOutcome :: LuaError e => Peeker e Outcome
peekOutcome idx = retrieving "test result" $ do
  Lua.ltype idx >>= \case
    Lua.TypeString  -> fmap Failure <$> peekString idx
    Lua.TypeBoolean -> do
      b <- toboolean idx
      return . pure $ if b then Success else Failure "???"
    _ -> failure <$> typeMismatchMessage "string or boolean" idx
