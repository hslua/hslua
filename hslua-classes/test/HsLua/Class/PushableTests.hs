{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      :  HsLua.Class.PushableTests
Copyright   :  © 2017-2021 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Test for the interoperability between haskell and lua.
-}
module HsLua.Class.PushableTests (tests) where

import Data.ByteString (ByteString)
import HsLua.Class.Pushable (Pushable (push))
import HsLua.Core (gettop, equal, nth)
import Foreign.StablePtr (castStablePtrToPtr, freeStablePtr, newStablePtr)

import Lua.Arbitrary ()
import Test.Tasty.HsLua (pushLuaExpr)
import Test.QuickCheck (Property)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Pushable"
  [ testGroup "pushing simple values to the stack"
    [ testCase "Boolean can be pushed correctly" $
      assertLuaEqual "true was not pushed"
        True
        "true"

    , testCase "Lua.Numbers can be pushed correctly" $
      assertLuaEqual "5::Lua.Number was not pushed"
        (5 :: Lua.Number)
        "5"

    , testCase "Lua.Integers can be pushed correctly" $
      assertLuaEqual "42::Lua.Integer was not pushed"
        (42 :: Lua.Integer)
        "42"

    , testCase "ByteStrings can be pushed correctly" $
      assertLuaEqual "string literal was not pushed"
        ("Hello!" :: ByteString)
        "\"Hello!\""

    , testCase "Unit is pushed as nil" $
      assertLuaEqual "() was not pushed as nil"
        ()
        "nil"

    , testCase "Pointer is pushed as light userdata" $
      let luaOp = do
            stblPtr <- Lua.liftIO $ newStablePtr (Just "5" :: Maybe String)
            push (castStablePtrToPtr stblPtr)
            res <- Lua.islightuserdata (-1)
            Lua.liftIO $ freeStablePtr stblPtr
            return res
      in assertBool "pointers must become light userdata"
         =<< Lua.run @Lua.Exception luaOp
    ]

  , testGroup "pushing a value increases stack size by one"
    [ testProperty "Lua.Integer"
      (prop_pushIncrStackSizeByOne :: Lua.Integer -> Property)
    , testProperty "Lua.Number"
      (prop_pushIncrStackSizeByOne :: Lua.Number -> Property)
    , testProperty "ByteString"
      (prop_pushIncrStackSizeByOne :: ByteString -> Property)
    , testProperty "String"
      (prop_pushIncrStackSizeByOne :: String -> Property)
    , testProperty "list of booleans"
      (prop_pushIncrStackSizeByOne :: [Bool] -> Property)
    ]
  ]

-- | Takes a message, haskell value, and a representation of that value as lua
-- string, assuming that the pushed values are equal within lua.
assertLuaEqual :: Pushable a => String -> a -> ByteString -> Assertion
assertLuaEqual msg x lit = assertBool msg =<< Lua.run @Lua.Exception
   (pushLuaExpr lit
   *> push x
   *> equal (nth 1) (nth 2))

prop_pushIncrStackSizeByOne :: Pushable a => a -> Property
prop_pushIncrStackSizeByOne x = monadicIO $ do
  (oldSize, newSize) <- run . Lua.run @Lua.Exception $
    ((,) <$> gettop <*> (push x *> gettop))
  assert (newSize == succ oldSize)
