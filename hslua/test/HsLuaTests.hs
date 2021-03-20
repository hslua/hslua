{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.Module
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Tests for HsLua.
-}
module HsLuaTests (tests) where

import Prelude hiding (concat)

import Control.Monad (void)
import Data.ByteString (append)
import Data.Data (Typeable)
import Data.Either (isLeft)
import HsLua as Lua
import System.Mem (performMajorGC)
import Test.Tasty.HsLua ( (=:), (?:), pushLuaExpr, shouldBeErrorMessageOf
                       , shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, testCase)

import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified HsLua.Core.Utf8 as Utf8

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Lua integration tests"
  [ testCase "print version" .
    run $ do
      openlibs
      void $ getglobal @Lua.Exception "assert"
      pushstring "Hello from "
      void $ getglobal @Lua.Exception "_VERSION"
      concat 2
      call 1 0

  , "getting a nested global works" ?: do
      pushLuaExpr @Lua.Exception "{greeting = 'Moin'}"
      setglobal "hamburg"

      getglobal' "hamburg.greeting"
      pushLuaExpr "'Moin'"
      equal (-1) (-2)

  , "setting a nested global works" ?: do
      let v = "Mitte"
      newtable
      setglobal @Lua.Exception "berlin"

      pushstring v
      setglobal' "berlin.neighborhood"
      v' <- getglobal' "berlin.neighborhood" *> tostring (-1)
      return (Just v == v')

  , testCase "table reading" .
    run @Lua.Exception $ do
      openbase
      let tableStr = "{firstname = 'Jane', surname = 'Doe'}"
      pushLuaExpr $ "setmetatable(" `append` tableStr `append` ", {'yup'})"
      void $ getfield top "firstname"
      firstname <- tostring top <* pop 1
      liftIO (assertEqual "Wrong value for firstname" (Just "Jane") firstname)

      pushstring "surname"
      rawget (-2)
      surname <- tostring top <* pop 1
      liftIO (assertEqual "Wrong value for surname" surname (Just "Doe"))

      hasMetaTable <- getmetatable (-1)
      liftIO (assertBool "getmetatable returned wrong result" hasMetaTable)
      rawgeti (-1) 1
      mt1 <- tostring top <* pop 1
      liftIO (assertEqual "Metatable content not as expected " mt1 (Just "yup"))

  , testGroup "Getting strings to and from the stack"
    [ testCase "unicode ByteString" $ do
        let val = T.pack "öçşiğüİĞı"
        val' <- run $ do
          pushstring (T.encodeUtf8 val)
          fmap T.decodeUtf8 `fmap` tostring 1
        assertEqual "Popped a different value or pop failed" (Just val) val'

    , testCase "ByteString should survive after GC/Lua destroyed" $ do
        (val, val') <- run $ do
          let v = "ByteString should survive"
          pushstring v
          v' <- tostring 1
          pop 1
          return (Just v, v')
        performMajorGC
        assertEqual "Popped a different value or pop failed" val val'
    , testCase "String with NUL byte should be pushed/popped correctly" $ do
        let str = "A\NULB"
        str' <- run $ pushstring (Char8.pack str) *> tostring 1
        assertEqual "Popped string is different than what's pushed"
          (Just str) (Char8.unpack <$> str')
    ]

  , testGroup "luaopen_* functions" $ map (uncurry testOpen)
    [ ("base", openbase)
    , ("debug", opendebug)
    , ("io", openio)
    , ("math", openmath)
    , ("os", openos)
    , ("package", openpackage)
    , ("string", openstring)
    , ("table", opentable)
    ]

  , testGroup "error handling"
    [ "catching error of a failing meta method" =:
      isLeft `shouldHoldForResultOf`
      let comp = do
            pushLuaExpr "setmetatable({}, {__index = error})"
            void $ getfield (-1) "foo"
      in try comp

    , "calling a function that errors throws exception" =:
      "[string \"return error('error message')\"]:1: error message"
      `shouldBeErrorMessageOf` do
        openbase
        loadstring "return error('error message')" *> call 0 1

    , let errTbl ="setmetatable({}, {__index = function(t, k) error(k) end})"
      in testGroup "error conversion"
      [ "throw custom exceptions" =: do
          let comp = do
                openlibs
                pushLuaExpr errTbl
                pushnumber 23
                void $ gettable (Lua.nth 2)
          result <- tryCustom comp
          result @?= Left (ExceptionWithNumber 23)

      , "catch custom exception in exposed function" =: do
          let frob = do
                openlibs
                pushLuaExpr errTbl
                pushnumber 42
                _ <- gettable (Lua.nth 2)
                return (NumResults 1)
          result <- tryCustom $ do
            openlibs
            pushHaskellFunction frob
            call (NumArgs 0) (NumResults 1)
          result @?= Left (ExceptionWithNumber 42)

      , "pass exception through Lua" =: do
          let frob :: LuaE CustomException NumResults
              frob = Catch.throwM (ExceptionWithMessage "borked")
          result <- tryCustom $ do
            pushHaskellFunction frob
            call (NumArgs 0) (NumResults 0)
          result @?= Left (ExceptionWithMessage "borked")
      ]
    ]
  ]

-------------------------------------------------------------------------------
-- luaopen_* functions

testOpen :: String -> Lua () -> TestTree
testOpen lib openfn = testCase ("open" ++ lib) $
  assertBool "opening the library failed" =<<
  run (openfn *> istable (-1))


-------------------------------------------------------------------------------
-- Custom exception handling

data CustomException =
    ExceptionWithNumber Lua.Number
  | ExceptionWithMessage String
  deriving (Eq, Show, Typeable)

instance Catch.Exception CustomException

instance LuaError CustomException where
  pushException = \case
    ExceptionWithMessage m -> pushstring (Utf8.fromString m)
    ExceptionWithNumber n  -> pushnumber n
  popException = do
    Lua.tonumber Lua.top >>= \case
      Just num -> do
        Lua.pop 1
        return (ExceptionWithNumber num)
      _        -> do
        l <- Lua.state
        msg <- Lua.liftIO (Lua.popErrorMessage l)
        return (ExceptionWithMessage (Utf8.toString msg))
  luaException = ExceptionWithMessage

tryCustom :: LuaE CustomException a -> IO (Either CustomException a)
tryCustom = Catch.try . Lua.run

-- instance Lua
-- customAlternative :: Lua a -> Lua a -> Lua a
-- customAlternative x y = Catch.try x >>= \case
--   Left (_ :: CustomException) -> y
--   Right x' -> return x'
