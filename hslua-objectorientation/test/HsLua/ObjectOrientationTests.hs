{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : HsLua.ObjectOrientationTests
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Test that conversions from and to the Lua stack are isomorphisms.
-}
module HsLua.ObjectOrientationTests (tests) where

import HsLua.Core
import HsLua.ObjectOrientation
import HsLua.Marshalling
import HsLua.Typing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HsLua ((=:), shouldBeResultOf, shouldBeErrorMessageOf)
import qualified Data.ByteString.Char8 as Char8

-- | Tests for HsLua object orientation.
tests :: TestTree
tests = testGroup "Object Orientation"
  [ testGroup "Sample product type"
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
        forcePeek $ peekUDGeneric typeFoo top

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
        forcePeek $ peekUDGeneric typeFoo top

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
        forcePeek $ peekUDGeneric typeFoo top

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

    , "absent properties are not included in `pairs`" =:
      [("num", "number"), ("str", "string"), ("show", "function")]
      `shouldBeResultOf` do
        openlibs
        pushUD typeQux $ Quux 1 "a"
        setglobal "a"
        OK <- dostring $ Char8.unlines
          [ "local result = {}"
          , "for k, v in pairs(a) do result[#result+1] = {k, type(v)} end"
          , "return result"
          ]
        forcePeek $ peekList (peekPair peekText peekText) top
    ]

  , testGroup "Bar type"
    [ "Modifying a table modifies the object" =:
      Bar [7, 8] `shouldBeResultOf` do
        openlibs
        pushUD typeBar $ Bar [7]
        setglobal "bar"
        OK <- dostring "table.insert(bar.nums, 8)"
        _ <- getglobal "bar"
        forcePeek $ peekUDGeneric typeBar top

    , "Use integer index in alias" =:
      42 `shouldBeResultOf` do
        openlibs
        pushUD typeBar $ Bar [42, 5, 23]
        setglobal "bar"
        OK <- dostring "return bar.first"
        forcePeek $ peekIntegral @Int top
    ]

  , testGroup "initType"
    [ "type table is added to the registry" =:
      TypeTable `shouldBeResultOf` do
        openlibs
        name <- initType typeBar
        getfield registryindex name

    , "type table is not in registry when uninitialized" =:
      TypeNil `shouldBeResultOf` do
        openlibs
        getfield registryindex (udName (typeBar @HsLua.Core.Exception))

    , "initializing does not affect the stack" =:
      0 `shouldBeResultOf` do
        openlibs
        before <- gettop
        _ <- initType typeBar
        after <- gettop
        return $ after - before
    ]

  , testGroup "lazy list"
    [ "Access an element of a lazy list stub" =:
      3 `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1,1,2,3,5,8]
        setglobal "list"
        _ <- dostring "return (list[4])"
        forcePeek $ peekIntegral @Int top

    , "Remaining list is not evaluated" =:
      2 `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1,1,2, Prelude.error "CRASH!"]
        setglobal "list"
        _ <- dostring "return (list[3])"
        forcePeek $ peekIntegral @Int top

    , "Out-of-bounds indices return nil" =:
      (TypeNil, TypeNil) `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1,4,9,16]
        setglobal "list"
        _ <- dostring "return list[0], list[5]"
        (,) <$> ltype (nth 1) <*> ltype (nth 2)

    , "Last evaled index is available in __lazylistindex" =:
      3 `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [9..17]
        setglobal "quuz"
        _ <- dostring "local foo = quuz[3]; return quuz.__lazylistindex"
        forcePeek $ peekIntegral @Int top

    , "__lazylistindex becomes `false` when all items are evaled" =:
      False `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1..3]
        setglobal "quuz"
        _ <- dostring "local foo = quuz[3]; return quuz.__lazylistindex"
        forcePeek $ peekBool top

    , "Input can be retrieved unchanged" =:
      LazyIntList [9..17] `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [9..17]
        setglobal "ninetofive"
        _ <- dostring "assert(ninetofive[3] == 11); return ninetofive"
        forcePeek $ peekUDGeneric typeLazyIntList top

    , "List is writable" =:
      LazyIntList [1, 4, 9, 16] `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [0,4,9,16]
        setglobal "list"
        OK <- dostring "list[1] = 1; return list"
        forcePeek $ peekUDGeneric typeLazyIntList top

    , "List can be extended" =:
      LazyIntList [1, 4, 9, 16, 25] `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1,4,9,16]
        setglobal "list"
        OK <- dostring "list[5] = 25; return list"
        forcePeek $ peekUDGeneric typeLazyIntList top

    , "List can be shortened" =:
      LazyIntList [1, 9, 27, 81] `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1, 9, 27, 81, 243]
        setglobal "list"
        OK <- dostring "list[5] = nil; return list"
        forcePeek $ peekUDGeneric typeLazyIntList top

    , "Setting element to nil shortenes the list" =:
      LazyIntList [1, 9, 27] `shouldBeResultOf` do
        openlibs
        pushUD typeLazyIntList $ LazyIntList [1, 9, 27, 81, 243]
        setglobal "list"
        OK <- dostring "list[4] = nil; return list"
        forcePeek $ peekUDGeneric typeLazyIntList top

    , "Infinite lists are ok" =:
      233 `shouldBeResultOf` do
        openlibs
        let fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
        pushUD typeLazyIntList $ LazyIntList fibs
        setglobal "fibs"
        dostring "return fibs[14]" >>= \case
          OK -> forcePeek $ peekIntegral @Int top
          _ -> failLua =<< forcePeek (peekString top)
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
      , "read subelement via integer alias" =:
        13.37 `shouldBeResultOf` do
          openlibs
          pushUD typeQux $ Quuz (Point 13.37 0) undefined
          setglobal "quuz"
          _ <- dostring "return quuz[1]"
          forcePeek $ peekRealFloat @Double top
      , "set subelement via integer alias" =:
        Point 42 1 `shouldBeResultOf` do
          openlibs
          pushUD typeQux $ Quuz (Point 1 1) undefined
          setglobal "quuz"
          _ <- dostring "quuz[1] = 42; return quuz.point"
          forcePeek $ peekPoint top
      , "non-aliased integer fields are nil" =:
        TypeNil `shouldBeResultOf` do
          openlibs
          pushUD typeQux (Quuz undefined undefined)
          setglobal "quuz"
          _ <- dostring "return quuz[3]"
          ltype top
      , "absent alias returns `nil`" =:
        TypeNil `shouldBeResultOf` do
          openlibs
          pushUD typeQux (Quux 9 "to five")
          setglobal "quux"
          dostring "return quux.x" >>= \case
            OK -> ltype top
            _ -> failLua =<< forcePeek (peekString top)
      , "alias can point to the element itself" =:
        9 `shouldBeResultOf` do
          openlibs
          pushUD typeLazyIntList (LazyIntList [1, 1, 1, 3, 5, 9, 17, 31])
          setglobal "tribonacci"
          dostring "return tribonacci.seq[6]" >>= \case
            OK -> forcePeek $ peekIntegral @Int top
            _ -> failLua =<< forcePeek (peekString top)
      ]
    ]
  ]

-- | Define a default UDType without bells and whistles.
deftype :: LuaError e
        => Name                              -- ^ type name
        -> [(Operation, HaskellFunction e)]  -- ^ operations
        -> [Member e (HaskellFunction e) a]  -- ^ methods
        -> UDType e (HaskellFunction e) a
deftype = deftypeGeneric pushHaskellFunction

-- | Pushes a userdata value of the given type.
pushUD
  :: LuaError e
  => UDTypeGeneric e fn a -> a -> LuaE e ()
pushUD = pushUDGeneric

-- | Define a (meta) operation on a type.
operation :: Operation -> HaskellFunction e -> (Operation, HaskellFunction e)
operation = (,)

-- | Sample product type
data Foo = Foo Int String
  deriving (Eq, Show)

-- | Specify behavior of Foo values in Lua.
typeFoo :: LuaError e => UDType e (HaskellFunction e) Foo
typeFoo = deftype "Foo"
  [ operation Tostring show' ]
  [ property "num" "some number"
      (pushIntegral, \(Foo n _) -> n)
      (peekIntegral, \(Foo _ s) n -> Foo n s)
  , readonly "str" "some string" (pushString, \(Foo _ s) -> s)
  , methodGeneric "show" show'
  ]
  where
    show' = do
      foo <- forcePeek $ peekUDGeneric typeFoo (nthBottom 1)
      pushString (show foo)
      return (NumResults 1)


newtype Bar = Bar [Int]
  deriving (Eq, Show)

typeBar :: LuaError e => UDType e (HaskellFunction e) Bar
typeBar = deftype "Bar" []
  [ property' "nums" (seqType integerType) "some numbers"
    (pushList pushIntegral, \(Bar nums) -> nums)
    (peekList peekIntegral, \(Bar _) nums -> Bar nums)
  , alias "first" "first element" ["nums", IntegerIndex 1]
  ]

newtype LazyIntList = LazyIntList { fromLazyIntList :: [Int] }
  deriving (Eq, Show)

typeLazyIntList :: LuaError e
                => UDTypeGeneric e (HaskellFunction e) LazyIntList
typeLazyIntList = deftypeGeneric' pushHaskellFunction "LazyIntList"
  [ operation Tostring $ do
      lazyList <- forcePeek $ peekUDGeneric typeLazyIntList (nthBottom 1)
      pushString (show lazyList)
      return (NumResults 1)
  ]
  [ alias "seq" "sequence" [] ]
  (listExtension
    ( (pushIntegral, fromLazyIntList)
    , (peekIntegral, \_ lst -> LazyIntList lst)
    ))

--
-- Sample sum type
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

pointType :: TypeSpec
pointType = recType
  [ ("x", numberType)
  , ("y", numberType)
  ]

showQux :: LuaError e => HaskellFunction e
showQux = do
  qux <- forcePeek $ peekQux (nthBottom 1)
  pushString $ show qux
  return (NumResults 1)

peekQux :: LuaError e => Peeker e Qux
peekQux = peekUDGeneric typeQux

typeQux :: LuaError e => UDType e (HaskellFunction e) Qux
typeQux = deftype "Qux"
  [ operation Tostring showQux ]
  [ methodGeneric "show" showQux
  , property' "num" integerType "some number"
      (pushIntegral, \case
          Quux n _ -> n
          Quuz _ n -> n)
      (peekIntegral, \case
          Quux _ s -> (`Quux` s)
          Quuz d _ -> Quuz d)

  , possibleProperty' "str" stringType "a string in Quux"
    (pushString, \case
        Quux _ s -> Actual s
        Quuz {}  -> Absent)
    (peekString, \case
        Quux n _ -> Actual . Quux n
        Quuz {}  -> const Absent)

  , possibleProperty' "point" pointType "a point in Quuz"
    (pushPoint, \case
        Quuz p _ -> Actual p
        Quux {}  -> Absent)
    (peekPoint, \case
        Quuz _ n -> Actual . (`Quuz` n)
        Quux {}  -> const Absent)

  , alias "x" "The x coordinate of a point in Quuz" ["point", "x"]
  , alias (IntegerIndex 1) "The x coordinate of a point in Quuz" ["point", "x"]
  ]
