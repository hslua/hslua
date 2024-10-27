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

  , "modify nested element" =:
    Quuy Quuz `shouldBeResultOf` do
      pushUD typeQux $ Quuy (Quux 5)
      pushQux Quuz
      setfield (nth 2) "qux"
      forcePeek $ peekQux top

  , "repeated modifications" =:
    Quuy Quuz `shouldBeResultOf` do
      pushUD typeQux $ Quuy (Quux 1)
      pushQux $ Quux 2
      setfield (nth 2) "qux"
      pushQux Quuz
      setfield (nth 2) "qux"
      forcePeek $ peekQux top

  , testGroup "stack usage"
    [ "pushing adds one element to the stack" =:
      1 `shouldBeResultOf` do
        before <- gettop
        pushUD typeQux $ Quuy (Quux 1)
        after <- gettop
        return $ after - before

    , "getting a field adds one element" =:
      1 `shouldBeResultOf` do
        pushUD typeQux $ Quuy (Quux 1)
        before <- gettop
        getfield top "qux"
        after <- gettop
        return $ after - before

    , "modifying a field does not affect the stack" =:
      0 `shouldBeResultOf` do
        pushUD typeQux $ Quuy (Quux 1)
        before <- gettop
        pushQux Quuz
        setfield (nth 2) "qux"
        after <- gettop
        return $ after - before

    , "peeking does not affect the stack" =:
      0 `shouldBeResultOf` do
        openlibs
        -- Create object with fields in caching table
        pushUD typeQux $ Quuy (Quux 1)
        pushQux Quuz
        setfield (nth 2) "qux"

        before <- gettop
        _ <- forcePeek $ peekQux top
        after <- gettop
        return $ after - before

    , "peeking unmodified element does not affect the stack" =:
      0 `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quuy (Quux 1)

        before <- gettop
        _ <- forcePeek $ peekQux top
        after <- gettop
        return $ after - before
    ]
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
  | Quuy Qux
  | Quuz
  deriving (Eq, Show)

peekQux :: LuaError e => Peeker e Qux
peekQux = peekUDGeneric typeQux

pushQux :: LuaError e => Pusher e Qux
pushQux = pushUD typeQux

typeQux :: LuaError e => UDSumTypeGeneric e (HaskellFunction e) Qux
typeQux = defsumtypeGeneric pushHaskellFunction "Qux"
  []
  []
  (\case
      Quux{} -> "Quux"
      Quuy{} -> "Quuy"
      Quuz{} -> "Quuz")
  [ defconstructor "Quux"
    "integer container"
    [ constructorProperty "int"
      integerType
      "some integer property"
      (pushIntegral, \case
          Quux int -> Actual int
          Quuy _   -> Absent
          Quuz     -> Absent)
      (peekIntegral, \case
          Quux _ -> Actual . Quux
          Quuy _ -> const Absent
          Quuz   -> const Absent)
    ]

  , defconstructor "Quuy"
    "nested Qux"
    [ constructorProperty "qux"
      "Qux"
      "nested Qux"
      (pushUD typeQux, \case
          Quux _  -> Absent
          Quuy qx -> Actual qx
          Quuz    -> Absent)
      (peekQux, \case
          Quux _ -> const Absent
          Quuy _ -> Actual . Quuy
          Quuz   -> const Absent)
    ]

  , defconstructor "Quuz"
    "property-less element"
    []
  ]
