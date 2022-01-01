{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.Tasty.HsLua
Copyright   : © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Utilities for testing of HsLua operations.
-}
module Test.Tasty.HsLua
  ( assertLuaBool
  , pushLuaExpr
  , shouldBeErrorMessageOf
  , shouldBeResultOf
  , shouldHoldForResultOf
  , (=:)
  , (?:)
  ) where

import Data.ByteString (ByteString, append)
import HsLua.Core
  (Lua, LuaE, LuaError, run, runEither, loadstring, call, multret)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
  (Assertion, HasCallStack, assertBool, assertFailure, testCase, (@?=))

import qualified HsLua.Core as Lua

-- | Takes a Lua expression as a 'ByteString', evaluates it and pushes
-- the result to the stack.
--
-- > -- will return "12"
-- > run $ do
-- >   pushLuaExpr "7 + 5"
-- >   tointeger top
pushLuaExpr :: LuaError e => ByteString -> LuaE e ()
pushLuaExpr expr = loadstring ("return " `append` expr) *> call 0 multret

-- | Takes a value and a 'Lua' operation and turns them into an
-- 'Assertion' which checks that the operation produces the given value.
shouldBeResultOf :: (HasCallStack, Eq a, Show a)
                 => a -> Lua a -> Assertion
shouldBeResultOf expected luaOp = do
  errOrRes <- runEither luaOp
  case errOrRes of
    Left (Lua.Exception msg) -> assertFailure $ "Lua operation failed with "
                                ++ "message: '" ++ msg ++ "'"
    Right res -> res @?= expected

-- | Checks whether a 'Lua' operation fails with the given string as
-- error message.
shouldBeErrorMessageOf :: (HasCallStack, Show a)
                       => String -> Lua a -> Assertion
shouldBeErrorMessageOf expectedErrMsg luaOp = do
  errOrRes <- runEither luaOp
  case errOrRes of
    Left (Lua.Exception msg) -> msg @?= expectedErrMsg
    Right res ->
      assertFailure ("Lua operation succeeded unexpectedly and returned "
                     ++ show res)

-- | Checks whether the return value of an operation holds for the given
-- predicate.
shouldHoldForResultOf :: (HasCallStack, Show a)
                      => (a -> Bool) -> Lua a -> Assertion
shouldHoldForResultOf predicate luaOp = do
  errOrRes <- runEither luaOp
  case errOrRes of
    Left (Lua.Exception msg) -> assertFailure $ "Lua operation failed with "
                                ++ "message: '" ++ msg ++ "'"
    Right res -> assertBool ("predicate doesn't hold for " ++ show res)
                            (predicate res)

-- | Checks whether the operation returns 'True'.
assertLuaBool :: HasCallStack => LuaE e Bool -> Assertion
assertLuaBool luaOp = assertBool "" =<< run luaOp

-- | Creates a new test case with the given name, checking whether the
-- operation returns 'True'.
luaTestBool :: HasCallStack => String -> LuaE e Bool -> TestTree
luaTestBool msg luaOp = testCase msg $
  assertBool "Lua operation returned false" =<< run luaOp

-- | Infix alias for 'testCase'.
(=:) :: String -> Assertion -> TestTree
(=:) = testCase
infix  3 =:

-- | Infix alias for 'luaTestBool'.
(?:) :: HasCallStack => String -> LuaE e Bool -> TestTree
(?:) = luaTestBool
infixr 3 ?:
