{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications     #-}
{-| Tests for error handling.
-}
module HsLua.Core.ErrorTests (tests) where

import Control.Applicative ((<|>), empty)
import Control.Exception
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Either (isLeft)
import HsLua.Core (Lua, failLua)
import HsLua.Core.Error ( LuaError, changeErrorType, popErrorMessage
                        , throwTypeMismatchError)
import HsLua.Core.Types (liftLua)
import Test.Tasty.HsLua ( (=:), (?:), shouldBeResultOf, shouldHoldForResultOf
                        , shouldBeErrorMessageOf)
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Error"
  [ "try catches errors" =:
    isLeft `shouldHoldForResultOf` Lua.try
      (failLua "test" :: Lua ())

  , "second alternative is used when first fails" ?:
    ((failLua "test" :: Lua Bool) <|> return True)

  , "Applicative.empty implementation throws an exception" =:
    isLeft `shouldHoldForResultOf` Lua.try (empty :: Lua ())

  , testGroup "changeErrorType"
    [ "catches error as different type in argument operation" =:
      Left (SampleException "message") `shouldBeResultOf`
      changeErrorType (Lua.try @SampleException @() $ failLua "message")

    , "passes value through on success" =:
      Just "plant" `shouldBeResultOf` do
        Lua.pushstring "plant"
        changeErrorType (Lua.tostring Lua.top)
    ]

  , testGroup "type mismatch"
    [ "got string" =:
      "number expected, got string" `shouldBeErrorMessageOf` do
        Lua.pushstring "moin"
        throwTypeMismatchError "number" Lua.top :: Lua ()
    , "got unnamed userdata" =:
      "number expected, got userdata" `shouldBeErrorMessageOf` do
        Lua.newhsuserdata ()
        throwTypeMismatchError "number" Lua.top :: Lua ()
    , "named userdata" =:
      "Bar expected, got Foo" `shouldBeErrorMessageOf` do
        Lua.newhsuserdata ()
        Lua.newudmetatable "Foo"
        Lua.setmetatable (Lua.nth 2)
        throwTypeMismatchError "Bar" Lua.top :: Lua ()

    , "missing value" =:
      "boolean expected, got no value" `shouldBeErrorMessageOf` do
        curtop <- Lua.gettop
        throwTypeMismatchError "boolean" (curtop + 1) :: Lua ()
    ]
  ]

newtype SampleException = SampleException ByteString
  deriving (Eq, Typeable, Show)

instance Exception SampleException

instance LuaError SampleException where
  popException = SampleException <$> liftLua popErrorMessage
  pushException (SampleException msg) = Lua.pushstring msg
  luaException = SampleException . Utf8.fromString
