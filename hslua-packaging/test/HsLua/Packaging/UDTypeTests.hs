{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.UDTypeTests
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.UDTypeTests (tests) where

import HsLua.Core
import HsLua.Packaging.Function
import HsLua.Packaging.Operation
import HsLua.Packaging.UDType
import HsLua.Marshalling
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import qualified Data.ByteString.Char8 as Char8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "UDType"
  [ testGroup "Foo type"
    [ "tostring" =:
      "Foo 7 \"seven\"" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 7 "seven"
        setglobal "foo"
        _ <- dostring "return tostring(foo)"
        peekText top >>= force

    , "show" =:
      "Foo 5 \"five\"" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 5 "five"
        setglobal "foo"
        _ <- dostring "return foo:show()"
        peekText top >>= force

    , "peek" =:
      Foo 37 "ananas" `shouldBeResultOf` do
        pushUD typeFoo $ Foo 37 "ananas"
        peekUD typeFoo top >>= force

    , "get number" =:
      (-1) `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo (-1) "a"
        setglobal "foo"
        dostring "return foo.num" >>= \case
          OK -> peekIntegral @Int top >>= force
          _ -> throwErrorAsException

    , "modify number" =:
      Foo (-1) "a" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 1 "a"
        setglobal "foo"
        OK <- dostring "foo.num = -1"
        TypeUserdata <- getglobal "foo"
        peekUD typeFoo top >>= force

    , "modify string" =:
      Foo 2 "c" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 2 "b"
        setglobal "foo"
        OK <- dostring "foo.str = 'c'"
        TypeUserdata <- getglobal "foo"
        peekUD typeFoo top >>= force

    , "pairs iterates over properties" =:
      ["num", "5", "str", "echo", "show", "function"] `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 5 "echo"
        setglobal "echo"
        OK <- dostring $ Char8.unlines
          [ "local result = {}"
          , "for k, v in pairs(echo) do"
          , "  table.insert(result, k)"
          , "  table.insert("
          , "    result,"
          , "    type(v) == 'function' and 'function' or tostring(v)"
          , "  )"
          , "end"
          , "return result"
          ]
        peekList peekText top >>= force
    ]
  ]

--
-- Sample type
--

data Foo = Foo Int String
  deriving (Eq, Show)

show' :: LuaError e => DocumentedFunction e
show' = defun "show"
  ### liftPure (show @Foo)
  <#> udparam typeFoo "foo" "Object"
  =#> functionResult pushString "string" "stringified foo"

typeFoo :: LuaError e => UDType e Foo
typeFoo = deftype "Foo"
  [ operation Tostring show'
  ]
  [ property "num" "some number"
      (pushIntegral, \(Foo n _) -> n)
      (peekIntegral, \(Foo _ s) n -> Foo n s)
  , property "str" "some string"
      (pushString, \(Foo _ s) -> s)
      (peekString, \(Foo n _) s -> Foo n s)
  , method show'
  ]
