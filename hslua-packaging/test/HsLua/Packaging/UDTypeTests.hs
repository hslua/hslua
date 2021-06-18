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
import Test.Tasty.HsLua ((=:), shouldBeResultOf, shouldBeErrorMessageOf)
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
        forcePeek $ peekText top

    , "show" =:
      "Foo 5 \"five\"" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 5 "five"
        setglobal "foo"
        _ <- dostring "return foo:show()"
        forcePeek $ peekText top

    , "peek" =:
      Foo 37 "ananas" `shouldBeResultOf` do
        pushUD typeFoo $ Foo 37 "ananas"
        forcePeek $ peekUD typeFoo top

    , "unknown properties have value `nil`" =:
      TypeNil `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo (-1) "a"
        setglobal "foo"
        dostring "return foo.does_not_exist" >>= \case
          OK -> ltype top
          _ -> throwErrorAsException

    , "get number" =:
      (-1) `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo (-1) "a"
        setglobal "foo"
        dostring "return foo.num" >>= \case
          OK -> forcePeek $ peekIntegral @Int top
          _ -> throwErrorAsException

    , "get number twice" =:
      8 `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 4 "d"
        setglobal "foo"
        dostring "return foo.num + foo.num" >>= \case
          OK -> forcePeek $ peekIntegral @Int top
          _ -> throwErrorAsException

    , "modify number" =:
      Foo (-1) "a" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 1 "a"
        setglobal "foo"
        OK <- dostring "foo.num = -1"
        TypeUserdata <- getglobal "foo"
        forcePeek $ peekUD typeFoo top

    , "get string" =:
      "lint" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 0 "lint"
        setglobal "foo"
        dostring "return foo.str" >>= \case
          OK -> forcePeek $ peekText top
          _ -> throwErrorAsException

    , "cannot change readonly string" =:
      "'str' is a read-only property." `shouldBeErrorMessageOf` do
        openlibs
        pushUD typeFoo $ Foo 2 "b"
        setglobal "foo"
        ErrRun <- dostring "foo.str = 'c'"
        throwErrorAsException :: Lua ()

    , "Can peek after getting read-only property" =:
      Foo 144 "gros" `shouldBeResultOf` do
        openlibs
        pushUD typeFoo $ Foo 144 "gros"
        setglobal "foo"
        OK <- dostring "bar = foo.str"
        _ <- getglobal "foo"
        forcePeek $ peekUD typeFoo top

    , "cannot change unknown property" =:
      "Cannot set unknown property." `shouldBeErrorMessageOf` do
        openlibs
        pushUD typeFoo $ Foo 11 "eleven"
        setglobal "foo"
        ErrRun <- dostring "foo.does_not_exist = nil"
        throwErrorAsException :: Lua ()

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

  , testGroup "Bar type"
    [ "Modifying a table modifies the object" =:
      Bar [7, 8] `shouldBeResultOf` do
        openlibs
        pushUD typeBar $ Bar [7]
        setglobal "bar"
        OK <- dostring "table.insert(bar.nums, 8)"
        _ <- getglobal "bar"
        forcePeek $ peekUD typeBar top
    ]

  , testGroup "possible properties"
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

    , "access Quux.num" =:
      "12" `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quux 12 "twelve"
        setglobal "quux"
        _ <- dostring "return quux.num"
        forcePeek $ peekText top

    , "access Quux.str" =:
      "thirteen!" `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quux 13 "thirteen"
        setglobal "quux"
        _ <- dostring "return quux.num"
        _ <- dostring "quux.str = quux.str .. '!'; return quux.str"
        forcePeek $ peekText top

    , testGroup "alias"
      [ "read subelement via alias" =:
        13.37 `shouldBeResultOf` do
          openlibs
          pushUD typeQux $ Quuz (Point 13.37 0) undefined
          setglobal "quuz"
          _ <- dostring "return quuz.x"
          forcePeek $ peekRealFloat @Double top
      , "set subelement via alias" =:
        Point 42 1 `shouldBeResultOf` do
          openlibs
          pushUD typeQux $ Quuz (Point 1 1) undefined
          setglobal "quuz"
          _ <- dostring "quuz.x = 42; return quuz.point"
          -- msg <- forcePeek $ peekString top
          -- liftIO $ putStrLn msg
          forcePeek $ peekPoint top
      ]
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

typeFoo :: LuaError e => UDType e Foo
typeFoo = deftype "Foo"
  [ operation Tostring show'
  ]
  [ property "num" "some number"
      (pushIntegral, \(Foo n _) -> n)
      (peekIntegral, \(Foo _ s) n -> Foo n s)
  , readonly "str" "some string" (pushString, \(Foo _ s) -> s)
  , method show'
  ]

newtype Bar = Bar [Int]
  deriving (Eq, Show)

typeBar :: LuaError e => UDType e Bar
typeBar = deftype "Bar" []
  [ property "nums" "some numbers"
    (pushList pushIntegral, \(Bar nums) -> nums)
    (peekList peekIntegral, \(Bar _) nums -> Bar nums)
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

typeQux :: LuaError e => UDType e Qux
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
