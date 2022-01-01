{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Packaging.UDTypeTests
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests for calling exposed Haskell functions.
-}
module HsLua.Packaging.UDTypeTests (tests) where

import HsLua.Core
import HsLua.Packaging.Function
import HsLua.Packaging.UDType
import HsLua.Marshalling
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HsLua ((=:), shouldBeResultOf)
import qualified Data.ByteString.Char8 as Char8

-- | Calling Haskell functions from Lua.
tests :: TestTree
tests = testGroup "DocumentedType"
  [ testGroup "Foo type"
    [ "show" =:
      "Foo 5 \"five\"" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 5 "five"
        setglobal "foo"
        _ <- dostring "return foo:show()"
        forcePeek $ peekText top

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
        forcePeek $ peekList peekText top
    ]

  , testGroup "Sum type"
    [ "tostring Quux" =:
      "Quux 11 \"eleven\"" `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quux 11 "eleven"
        setglobal "quux"
        _ <- dostring "return tostring(quux)"
        forcePeek $ peekText top
    , "show Quux" =:
      "Quux 11 \"eleven\"" `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quux 11 "eleven"
        setglobal "quux"
        _ <- dostring "return quux:show()"
        forcePeek $ peekText top
    ]
  ]

--
-- Sample types
--

data Foo = Foo Int String
  deriving (Eq, Show)

show' :: LuaError e => DocumentedFunction e
show' = defun "show"
  ### liftPure (show @Foo)
  <#> udparam typeFoo "foo" "Object"
  =#> functionResult pushString "string" "stringified foo"

typeFoo :: LuaError e => DocumentedType e Foo
typeFoo = deftype "Foo"
  [ operation Tostring show'
  ]
  [ property "num" "some number"
      (pushIntegral, \(Foo n _) -> n)
      (peekIntegral, \(Foo _ s) n -> Foo n s)
  , readonly "str" "some string" (pushString, \(Foo _ s) -> s)
  , method show'
  ]

--
-- Sum Type
--
data Qux
  = Quux Int String
  | Quuz Point Int
  deriving (Eq, Show)

data Point = Point Double Double
  deriving (Eq, Show)

pushPoint :: LuaError e => Pusher e Point
pushPoint (Point x y) = do
  newtable
  pushName "x" *> pushRealFloat x *> rawset (nth 3)
  pushName "y" *> pushRealFloat y *> rawset (nth 3)

peekPoint :: LuaError e => Peeker e Point
peekPoint idx = do
  x <- peekFieldRaw peekRealFloat "x" idx
  y <- peekFieldRaw peekRealFloat "y" idx
  return $ x `seq` y `seq` Point x y

showQux :: LuaError e => DocumentedFunction e
showQux = defun "show"
  ### liftPure (show @Qux)
  <#> parameter peekQux "qux" "qux" "Object"
  =#> functionResult pushString "string" "stringified Qux"

peekQux :: LuaError e => Peeker e Qux
peekQux = peekUD typeQux

typeQux :: LuaError e => DocumentedType e Qux
typeQux = deftype "Qux"
  [ operation Tostring showQux ]
  [ method showQux
  , property "num" "some number"
      (pushIntegral, \case
          Quux n _ -> n
          Quuz _ n -> n)
      (peekIntegral, \case
          Quux _ s -> (`Quux` s)
          Quuz d _ -> Quuz d)

  , possibleProperty "str" "a string in Quux"
    (pushString, \case
        Quux _ s -> Actual s
        Quuz {}  -> Absent)
    (peekString, \case
        Quux n _ -> Actual . Quux n
        Quuz {}  -> const Absent)

  , possibleProperty "point" "a point in Quuz"
    (pushPoint, \case
        Quuz p _ -> Actual p
        Quux {}  -> Absent)
    (peekPoint, \case
        Quuz _ n -> Actual . (`Quuz` n)
        Quux {}  -> const Absent)

  , alias "x" "The x coordinate of a point in Quuz" ["point", "x"]
  ]
