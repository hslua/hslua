{-
Copyright © 2018-2020 Albert Krewinkel

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
{-| Tests for utility types and functions
-}
module Foreign.Lua.UtilTests (tests) where

import Data.Either (isLeft, isRight)
import Foreign.Lua
import Test.HsLua.Util ( (?:), (=:), pushLuaExpr, shouldBeResultOf
                       , shouldBeErrorMessageOf, shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Utilities"
  [ "Optional return the value if it exists" =:
    (Just "Moin" :: Maybe String) `shouldBeResultOf` do
      push ("Moin" :: String)
      fromOptional <$> peek stackTop

  , "Optional can deal with nil values" =:
    (Nothing :: Maybe String) `shouldBeResultOf` do
      pushnil
      fromOptional <$> peek stackTop

  , "Optional can deal with nonexistent (none) values" =:
    Nothing `shouldBeResultOf`
      fmap fromOptional (peek (nthFromBottom 200) :: Lua (Optional String))

  , "raiseError causes a Lua error" =:
    "test error message" `shouldBeErrorMessageOf` do
      pushHaskellFunction (raiseError ("test error message" :: String))
      call 0 0
      return ()

  , testGroup "runEither"
    [ "Lua errors are caught" =:
      isLeft `shouldHoldForResultOf`
        liftIO (runEither' (push True *> peek (-1) :: Lua String))

    , "error-less code gives 'Right'" =:
      isRight `shouldHoldForResultOf`
        liftIO (runEither' (push True *> peek (-1) :: Lua Bool))
    ]

  , testGroup "peekEither"
    [ "return right result on success" =:
      Right (5 :: Lua.Integer) `shouldBeResultOf` do
        pushinteger 5
        peekEither stackTop

    , "return error message on failure" =:
      Left "Could not read list: expected integer, got 'false' (boolean)"
      `shouldBeResultOf` do
        pushLuaExpr "{1, false}"
        peekEither stackTop :: Lua (Either String [Lua.Integer])
    ]

  , testGroup "popValue"
    [ "value is retrieved and popped" =:
      (-1, "ocean" :: String) `shouldBeResultOf` do
        Lua.pushstring "ocean"
        oldTop <- Lua.gettop
        value <- Lua.popValue
        newTop <- Lua.gettop
        return (newTop - oldTop, value)

    , "value is popped even on error" =:
      (Left (-1) :: Either Lua.StackIndex Lua.Number) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        oldTop <- Lua.gettop
        value <- Lua.try Lua.popValue
        newTop <- Lua.gettop
        let stackDiff = newTop - oldTop
        return $ case value of
          Left _ -> Left stackDiff
          Right x -> Right x

    , "error messages equals that of peek" ?: do
        Lua.pushstring "not a number"
        p1 <- Lua.try (Lua.peek Lua.stackTop :: Lua Lua.Integer)
        p2 <- Lua.try (Lua.popValue :: Lua Lua.Integer)
        return (p1 == p2)
    ]
  ]


runEither' :: Lua a -> IO (Either Lua.Exception a)
runEither' = runEither
