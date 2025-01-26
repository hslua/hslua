{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : HsLua.ObjectOrientation.SumTypeTests
Copyright   : © 2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
-}
module HsLua.ObjectOrientation.SumTypeTests (tests) where

import HsLua.Core
import HsLua.ObjectOrientation
import HsLua.ObjectOrientation.SumType
import HsLua.Marshalling
import HsLua.Typing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)

-- | Tests for HsLua object orientation.
tests :: TestTree
tests = testGroup "Sample sum type"
  [ "element tag" =:
    "Quux" `shouldBeResultOf` do
      openlibs
      pushUD typeQux $ Quux 11
      getfield top "tag"
      forcePeek $ peekText top

  , "get property" =:
    17 `shouldBeResultOf` do
      pushUD typeQux $ Quux 17
      getfield top "int"
      forcePeek $ peekIntegral @Int top

  , "peek element" =:
    Quux 19 `shouldBeResultOf` do
      pushUD typeQux $ Quux 7
      pushinteger 19
      setfield (nth 2) "int"
      forcePeek $ peekQux top
  ]

-- | Pushes a userdata value of the given type.
pushUD
  :: UDTypeExtension e a extension
  => UDTypeGeneric e fn a extension -> a -> LuaE e ()
pushUD = pushUDGeneric (const (pure ()))

--
-- Sample sum type
--
data Qux
  = Quux Int
  | Quuz String
  deriving (Eq, Show)

peekQux :: LuaError e => Peeker e Qux
peekQux = peekUDGeneric typeQux

typeQux :: LuaError e => UDSumTypeGeneric e (HaskellFunction e) Qux
typeQux = defsumtypeGeneric pushHaskellFunction "Qux"
  []
  []
  (\case
      Quux{} -> "Quux"
      Quuz{} -> "Quuz")
  [ defconstructor "Quux"
    "integer container"
    [ constructorProperty "int"
      integerType
      "some integer property"
      (pushIntegral, \case
          Quux int -> Actual int
          Quuz _   -> Absent)
      (peekIntegral, \case
          Quux _ -> Actual . Quux
          Quuz _ -> const Absent)
    ]
  ]
