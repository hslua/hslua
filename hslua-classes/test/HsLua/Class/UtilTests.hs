{-# LANGUAGE OverloadedStrings #-}
{-|
Module      :  HsLua.Class.UtilTests
Copyright   :  © 2017-2023 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb@hslua.org>
Stability   :  stable
Portability :  portable

Tests for utility types and functions
-}
module HsLua.Class.UtilTests (tests) where

import Data.Either (isLeft, isRight)
import HsLua.Class.Peekable
import HsLua.Class.Pushable
import HsLua.Class.Util
import HsLua.Core as Lua
import Test.Tasty.HsLua ( (?:), (=:), pushLuaExpr, shouldBeResultOf
                       , shouldBeErrorMessageOf, shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Utilities"
  [ "Optional return the value if it exists" =:
    (Just "Moin" :: Maybe String) `shouldBeResultOf` do
      push ("Moin" :: String)
      fromOptional <$> peek top

  , "Optional can deal with nil values" =:
    (Nothing :: Maybe String) `shouldBeResultOf` do
      pushnil
      fromOptional <$> peek top

  , "Optional can deal with nonexistent (none) values" =:
    Nothing `shouldBeResultOf`
      fmap fromOptional (peek (nthBottom 20) :: Lua (Optional String))

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
        peekEither top

    , "return error message on failure" =:
      let msg = "integer expected, got boolean"
            <> "\n\twhile retrieving index 2"
            <> "\n\twhile retrieving list"
      in
      Left (Lua.Exception msg)
      `shouldBeResultOf` do
        pushLuaExpr "{1, false}"
        peekEither top :: Lua (Either Lua.Exception [Lua.Integer])
    ]

  , testGroup "popValue"
    [ "value is retrieved and popped" =:
      (-1, "ocean" :: String) `shouldBeResultOf` do
        Lua.pushstring "ocean"
        oldTop <- Lua.gettop
        value <- popValue
        newTop <- Lua.gettop
        return (newTop - oldTop, value)

    , "value is popped even on error" =:
      (Left (-1) :: Either Lua.StackIndex Lua.Number) `shouldBeResultOf` do
        Lua.pushstring "not a number"
        oldTop <- Lua.gettop
        value <- Lua.try popValue
        newTop <- Lua.gettop
        let stackDiff = newTop - oldTop
        return $ case value of
          Left _ -> Left stackDiff
          Right x -> Right x

    , "error messages equals that of peek" ?: do
        Lua.pushstring "not a number"
        p1 <- Lua.try (peek Lua.top :: Lua Lua.Integer)
        p2 <- Lua.try (popValue :: Lua Lua.Integer)
        return (p1 == p2)
    ]
  ]


runEither' :: Lua a -> IO (Either Lua.Exception a)
runEither' = runEither
