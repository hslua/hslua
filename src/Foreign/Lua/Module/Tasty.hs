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
import qualified Test.Tasty.HUnit as Tasty


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
      return $ Tasty.testGroup fp $ map dummyTestTree results
    else Lua.throwTopMessage

data TestReport
  = TestReport Tasty.TestName TestResult

instance Peekable TestReport where
  peek idx = do
    name   <- Lua.getfield idx "name"   *> Lua.popValue
    result <- Lua.getfield idx "result" *> Lua.popValue
    return $ TestReport name result

dummyTestTree :: TestReport -> Tasty.TestTree
dummyTestTree (TestReport name outcome) =
  case outcome of
    TestSuccess success -> Tasty.testCase name (Tasty.assertBool "" success)
    TestFailure msg     -> Tasty.testCase name (Tasty.assertFailure msg)
    TestGroup results   -> Tasty.testGroup name (map dummyTestTree results)

data TestResult
  = TestSuccess Bool
  | TestFailure String
  | TestGroup [TestReport]

instance Peekable TestResult where
  peek = peekTestResult

peekTestResult :: StackIndex -> Lua TestResult
peekTestResult idx = do
  ty <- Lua.ltype idx
  case ty of
    Lua.TypeBoolean -> TestSuccess <$> Lua.peek idx
    Lua.TypeString  -> TestFailure <$> Lua.peek idx
    Lua.TypeTable   -> TestGroup   <$> Lua.peekList idx
    _ -> do
      s <- Text.unpack . Text.Encoding.decodeUtf8 <$> Lua.tostring' idx
      Lua.throwException ("not a test result: " ++ s)
