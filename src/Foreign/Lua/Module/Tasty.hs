{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Foreign.Lua.Module.Tasty
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires TemplateHaskell

Expose functions of the tasty Haskell test-framework.
-}
module Foreign.Lua.Module.Tasty
  ( pushModule
  , testsFromFile
  )
where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.FileEmbed
import Foreign.Lua (Lua, NumResults, Peekable, StackIndex)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Foreign.Lua as Lua
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Providers as Tasty


-- | Tasty Lua script
tastyScript :: ByteString
tastyScript = $(embedFile "tasty.lua")

-- | Push the Aeson module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  result <- Lua.dostring tastyScript
  if result == Lua.OK
    then return 1
    else Lua.throwTopMessage
{-# INLINABLE pushModule #-}

testsFromFile :: FilePath -> Lua Tasty.TestTree
testsFromFile fp =  do
  Lua.openlibs
  Lua.requirehs "tasty" (void pushModule)
  res <- Lua.dofile fp
  if res == Lua.OK
    then do
      results <- Lua.peekList Lua.stackTop
      return $ Tasty.testGroup fp $ map testTree results
    else Lua.throwTopMessage

testTree :: Tree -> Tasty.TestTree
testTree (Tree name tree) =
  case tree of
    SingleTest outcome -> Tasty.singleTest name outcome
    TestGroup results  -> Tasty.testGroup name (map testTree results)

data Tree = Tree Tasty.TestName UnnamedTree

instance Peekable Tree where
  peek idx = do
    name   <- Lua.getfield idx "name"   *> Lua.popValue
    result <- Lua.getfield idx "result" *> Lua.popValue
    return $ Tree name result

instance Tasty.IsTest Outcome where
  run _ tr _ = return $ case tr of
    Success     -> Tasty.testPassed ""
    Failure msg -> Tasty.testFailed msg
  testOptions = return []


data UnnamedTree
  = SingleTest Outcome
  | TestGroup [Tree]

instance Peekable UnnamedTree where
  peek = peekTree

peekTree :: StackIndex -> Lua UnnamedTree
peekTree idx = do
  ty <- Lua.ltype idx
  case ty of
    Lua.TypeTable   -> TestGroup   <$> Lua.peekList idx
    _               -> SingleTest  <$> Lua.peek idx

-- | Test outcome
data Outcome = Success | Failure String

instance Peekable Outcome where
  peek idx = do
    ty <- Lua.ltype idx
    case ty of
      Lua.TypeString  -> Failure <$> Lua.peek idx
      Lua.TypeBoolean -> do
        b <- Lua.peek idx
        return $ if b then Success else Failure "???"
      _ -> do
        s <- Text.unpack . Text.Encoding.decodeUtf8 <$> Lua.tostring' idx
        Lua.throwException ("not a test result: " ++ s)
