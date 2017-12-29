{-
Copyright © 2017 Albert Krewinkel

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
{-# LANGUAGE OverloadedStrings #-}
{-| Tests for lua -}
module Foreign.LuaTest (tests) where

import Prelude hiding (concat)

import Control.Applicative ((<|>), empty)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Data.Monoid ((<>))
import Foreign.Lua
import System.Mem (performMajorGC)
import Test.HsLua.Util (luaTestCase, pushLuaExpr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua integration tests"
  [ testCase "print version" .
    runLua $ do
      openlibs
      getglobal "assert"
      push ("Hello from " :: ByteString)
      getglobal "_VERSION"
      concat 2
      call 1 0

  , testCase "functions stored in / retrieved from registry" .
    runLua $ do
      pushLuaExpr "function() return 2 end, function() return 1 end"
      idx1 <- ref registryindex
      idx2 <- ref registryindex
      liftIO . assertBool "functions are removed from stack"
        =<< fmap (TypeFunction /=) (ltype (-1))

      -- get functions from registry
      rawgeti registryindex idx1
      call 0 1
      r1 <- peek (-1) :: Lua LuaInteger
      liftIO (assertEqual "received function returned wrong value" 1 r1)

      rawgeti registryindex idx2
      call 0 1
      r2 <- peek (-1) :: Lua LuaInteger
      liftIO (assertEqual "received function returned wrong value" 2 r2)

      -- delete references
      unref registryindex idx1
      unref registryindex idx2

  , luaTestCase "getting a nested global works" $ do
      pushLuaExpr "{greeting = 'Moin'}"
      setglobal "hamburg"

      getglobal' "hamburg.greeting"
      pushLuaExpr "'Moin'"
      equal (-1) (-2)

  , luaTestCase "setting a nested global works" $ do
      let v = "Mitte"
      newtable
      setglobal "berlin"

      pushstring v
      setglobal' "berlin.neighborhood"
      v' <- getglobal' "berlin.neighborhood" *> tostring (-1)
      return (v == v')

  , testCase "table reading" .
    runLua $ do
      openbase
      let tableStr = "{firstname = 'Jane', surname = 'Doe'}"
      pushLuaExpr $ "setmetatable(" <> tableStr <> ", {'yup'})"
      getfield (-1) "firstname"
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
        val' <- runLua $ do
          pushstring (T.encodeUtf8 val)
          T.decodeUtf8 `fmap` tostring 1
        assertEqual "Popped a different value or pop failed" val val'

    , testCase "ByteString should survive after GC/Lua destroyed" $ do
        (val, val') <- runLua $ do
          let v = B.pack "ByteString should survive"
          pushstring v
          v' <- tostring 1
          pop 1
          return (v, v')
        performMajorGC
        assertEqual "Popped a different value or pop failed" val val'
    , testCase "String with NUL byte should be pushed/popped correctly" $ do
        let str = "A\NULB"
        str' <- runLua $ pushstring (B.pack str) *> tostring 1
        assertEqual "Popped string is different than what's pushed"
          str (B.unpack str')
    ]

  , testGroup "luaopen_* functions" $ map (uncurry testOpen)
    [ ("debug", opendebug)
    , ("io", openio)
    , ("math", openmath)
    , ("os", openos)
    , ("package", openpackage)
    , ("string", openstring)
    , ("table", opentable)
    ]
  , testGroup "luaopen_base returns the right number of tables" testOpenBase

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
              else throwLuaError "Error in words function."
      in assertEqual "greeting function failed"
          (Right ["Caffeine", "induced", "nonsense"]) =<< runLuaEither comp

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
         runLuaEither comp
    ]

  , testGroup "error handling"
    [ testCase "lua errors are caught" $
      assertBool "error was not intercepted" . isLeft =<<
      runLuaEither (push True *> peek (-1) :: Lua String)

    , testCase "error-less code gives in 'Right' result" $
      assertBool "error was not intercepted" . isRight =<<
      runLuaEither (push True *> peek (-1) :: Lua Bool)

    , testCase "catching lua errors within the lua type" $
      assertBool "No error was thrown" . isLeft
        =<< (runLua $ tryLua (throwLuaError "test"))

    , testCase "second alternative is used when first fails" $
      assertEqual "alternative failed" (Right True) =<<
      runLuaEither (throwLuaError "test" <|> return True)

    , testCase "Applicative.empty implementation throws an exception" $
      assertBool "empty doesn't throw" . isLeft =<< runLuaEither empty

    , testCase "catching error of a failing meta method" $
      assertBool "compuation was expected to fail" . isLeft =<<
      let comp = do
            pushLuaExpr "setmetatable({}, {__index = error})"
            getfield (-1) "foo" :: Lua ()
      in runLuaEither comp

    , testCase "calling a function that errors throws exception" $
      let msg = "error message"
          luaCode = "return error('" ++ msg ++ "')"
          err =  "[string \"" ++ luaCode ++ "\"]:1: " ++ msg
          errTest x = x == (Left (LuaException msg)) || -- LuaJIT
                      x == (Left (LuaException err))    -- default Lua
      in assertBool "problem in error" . errTest =<<
      (runLuaEither $ do
          openbase
          res <- loadstring luaCode
          when (res == OK) $ call 0 0)
    ]
  ]

--------------------------------------------------------------------------------
-- luaopen_* functions

testOpen :: String -> Lua () -> TestTree
testOpen lib openfn = testCase ("open" ++ lib) $
  assertBool "opening the library failed" =<<
  runLua (openfn *> istable (-1))

testOpenBase :: [TestTree]
testOpenBase = (:[]) .
  testCase "openbase" $
  assertBool "loading base didn't push the expected number of tables" =<<
  (runLua $ do
    -- openbase returns one table in lua 5.2 and later, but two in 5.1
    openbase
    getglobal "_VERSION"
    version <- peek (-1) <* pop 1
    if version == ("Lua 5.1" :: ByteString)
      then (&&) <$> istable (-1) <*> istable (-2)
      else istable (-1))
