{-
Copyright © 2017-2021 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Tests for HsLua -}
module HsLuaTests (tests) where

import Prelude hiding (concat)

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.ByteString (ByteString)
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
import qualified HsLua.Utf8 as Utf8

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua integration tests"
  [ testCase "print version" .
    run $ do
      openlibs
      void $ getglobal "assert"
      push ("Hello from " :: ByteString)
      void $ getglobal "_VERSION"
      concat 2
      call 1 0

  , "getting a nested global works" ?: do
      pushLuaExpr "{greeting = 'Moin'}"
      setglobal "hamburg"

      getglobal' "hamburg.greeting"
      pushLuaExpr "'Moin'"
      equal (-1) (-2)

  , "setting a nested global works" ?: do
      let v = "Mitte"
      newtable
      setglobal "berlin"

      pushstring v
      setglobal' "berlin.neighborhood"
      v' <- getglobal' "berlin.neighborhood" *> tostring (-1)
      return (Just v == v')

  , testCase "table reading" .
    run $ do
      openbase
      let tableStr = "{firstname = 'Jane', surname = 'Doe'}"
      pushLuaExpr $ "setmetatable(" <> tableStr <> ", {'yup'})"
      void $ getfield (-1) "firstname"
      firstname <- peek (-1) <* pop 1 :: Lua ByteString
      liftIO (assertEqual "Wrong value for firstname" "Jane" firstname)

      push ("surname" :: ByteString)
      rawget (-2)
      surname <- peek (-1) <* pop 1 :: Lua ByteString
      liftIO (assertEqual "Wrong value for surname" surname "Doe")

      hasMetaTable <- getmetatable (-1)
      liftIO (assertBool "getmetatable returned wrong result" hasMetaTable)
      rawgeti (-1) 1
      mt1 <- peek (-1) <* pop 1 :: Lua ByteString
      liftIO (assertEqual "Metatable content not as expected " mt1 "yup")

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

  , testGroup "C functions"
    [ testCase "Registering a C function and calling it from Lua" $
      let comp :: Lua [String]
          comp = do
            fn <- newCFunction (return . words :: String -> Lua [String])
            register "words" fn
            res <- dostring "return words('Caffeine induced nonsense')"
            freeCFunction fn
            if res == OK
              then peek (-1)
              else throwException "Error in words function."
      in assertEqual "greeting function failed"
          (Right ["Caffeine", "induced", "nonsense"])
          =<< (runEither comp :: IO (Either Lua.Exception [String]))

    , testCase "pushing a C closure to and calling it from Lua" $
      -- Closures would usually be defined on the Haskell side, unless the
      -- upvalues cannot be read from the stack (e.g., a lua function).
      let greeter :: String -> HaskellFunction
          greeter greetee = do
            greeting <- peek (upvalueindex 1)
            push (greeting ++ (", " :: String) ++ greetee ++ ("!" :: String))
            return 1

          comp :: Lua String
          comp = do
            fn <- newCFunction (greeter "World")
            push ("Hello" :: String)
            pushcclosure fn 1
            call 0 multret
            freeCFunction fn
            peek (-1)

      in assertEqual "greeting function failed" (Right "Hello, World!") =<<
         (runEither comp :: IO (Either Lua.Exception String))
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
          let frob n = do
                pushLuaExpr errTbl
                pushnumber n
                void $ gettable (Lua.nth 2)
          result <- tryCustom $ do
            openlibs
            registerHaskellFunction "frob" frob
            callFunc "frob" (Lua.Number 42) :: Lua ()
          result @?= Left (ExceptionWithNumber 42)

      , "pass exception through Lua" =: do
          let frob = Catch.throwM (ExceptionWithMessage "borked") :: Lua ()
          result <- tryCustom $ do
            pushHaskellFunction frob
            call 0 1
          result @?= Left (ExceptionWithMessage "borked")

      , "failing peek" =: do
          let msg = "expected integer, got '1.1' (number)"
          result <- tryCustom $ do
            pushnumber 1.1
            peek top :: Lua Lua.Integer
          result @?= Left (ExceptionWithMessage msg)

      , "alternative" =: do
          result <- tryCustom $ do
            pushstring "NaN"
            pushnumber 13.37
            (<|>) (peek (nth 2) :: Lua Number)
                  (peek top :: Lua Number)
          result @?= Right 13.37
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

customErrorConversion :: Lua.ErrorConversion
customErrorConversion = Lua.ErrorConversion
  { errorToException = errorToCustomException
  , addContextToException = const id
  , alternative = customAlternative
  , exceptionToError = flip Catch.catch $ \case
      ExceptionWithMessage m -> raiseError (Utf8.fromString m)
      ExceptionWithNumber n  -> raiseError n
  }

errorToCustomException :: Lua.State -> IO a
errorToCustomException l = Lua.unsafeRunWith l $
  Lua.tonumber Lua.top >>= \case
    Just num -> do
      Lua.pop 1
      Catch.throwM (ExceptionWithNumber num)
    _        -> do
      msg <- Lua.liftIO (Lua.errorMessage l)
      Catch.throwM (ExceptionWithMessage (Utf8.toString msg))

tryCustom :: Lua a -> IO (Either CustomException a)
tryCustom = Catch.try . Lua.run' customErrorConversion

customAlternative :: Lua a -> Lua a -> Lua a
customAlternative x y = Catch.try x >>= \case
  Left (_ :: CustomException) -> y
  Right x' -> return x'
