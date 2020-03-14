{-
Copyright © 2017-2020 Albert Krewinkel

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
{-| Tests for HsLua -}
module Foreign.LuaTests (tests) where

import Prelude hiding (concat)

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Foreign.Lua as Lua
import System.Mem (performMajorGC)
import Test.HsLua.Util ( (=:), (?:), pushLuaExpr, shouldBeErrorMessageOf
                       , shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua integration tests"
  [ testCase "print version" .
    run $ do
      openlibs
      getglobal "assert"
      push ("Hello from " :: ByteString)
      getglobal "_VERSION"
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
          (Right ["Caffeine", "induced", "nonsense"]) =<< runEither comp

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
         runEither comp
    ]

  , testGroup "error handling"
    [ "catching error of a failing meta method" =:
      isLeft `shouldHoldForResultOf`
      let comp = do
            pushLuaExpr "setmetatable({}, {__index = error})"
            getfield (-1) "foo" :: Lua ()
      in try comp

    , "calling a function that errors throws exception" =:
      "[string \"return error('error message')\"]:1: error message"
      `shouldBeErrorMessageOf` do
        openbase
        loadstring "return error('error message')" *> call 0 1
    ]
  ]

--------------------------------------------------------------------------------
-- luaopen_* functions

testOpen :: String -> Lua () -> TestTree
testOpen lib openfn = testCase ("open" ++ lib) $
  assertBool "opening the library failed" =<<
  run (openfn *> istable (-1))
