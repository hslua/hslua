{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  HsLua.CoreTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for Lua C API-like functions.
-}
module HsLua.CoreTests (tests) where

import Prelude hiding (compare)

import Data.ByteString (append)
import Data.Maybe (fromMaybe)
import Lua.Lib (luaopen_debug)
import HsLua.Core as Lua
import HsLua.Core.Types (toType)
import Lua.Arbitrary ()
import Test.Tasty.HsLua ( (?:), (=:), shouldBeErrorMessageOf, shouldBeResultOf
                        , shouldHoldForResultOf, pushLuaExpr )
import Test.QuickCheck (Property, (.&&.))
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Prelude
import qualified Data.ByteString as B
import qualified HsLua.Core.AuxiliaryTests
import qualified HsLua.Core.ClosuresTests
import qualified HsLua.Core.ErrorTests
import qualified HsLua.Core.PackageTests
import qualified HsLua.Core.PrimaryTests
import qualified HsLua.Core.RunTests
import qualified HsLua.Core.TraceTests
import qualified HsLua.Core.UnsafeTests
import qualified HsLua.Core.UserdataTests
import qualified Foreign.Marshal as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Test.QuickCheck.Monadic as QCMonadic


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Core module"
  [ HsLua.Core.ErrorTests.tests
  , HsLua.Core.AuxiliaryTests.tests
  , testGroup "copy"
    [ "copies stack elements using positive indices" ?: do
        pushLuaExpr @Lua.Exception "5, 4, 3, 2, 1"
        copy 4 3
        rawequal (nthBottom 4) (nthBottom 3)

    , "copies stack elements using negative indices" ?: do
        pushLuaExpr @Lua.Exception "5, 4, 3, 2, 1"
        copy (-1) (-3)
        rawequal (-1) (-3)
    ]

  , testGroup "insert"
    [ "inserts stack elements using positive indices" ?: do
        pushLuaExpr @Lua.Exception "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert 4
        movedEl <- tointeger (nthBottom 4)
        newTop <- tointeger (nth 1)
        return (movedEl == Just 9 && newTop == Just 8)

    , "inserts stack elements using negative indices" ?: do
        pushLuaExpr @Lua.Exception "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert (-6)
        movedEl <- tointeger (nth 6)
        newTop <- tointeger (nth 1)
        return (movedEl == Just 9 && newTop == Just 8)
    ]

  , testCase "absindex" . run @Lua.Exception $ do
      pushLuaExpr "1, 2, 3, 4"
      liftIO . assertEqual "index from bottom doesn't change" (nthBottom 3)
        =<< absindex (nthBottom 3)
      liftIO . assertEqual "index from top is made absolute" (nthBottom 2)
        =<< absindex (nth 3)
      liftIO . assertEqual "pseudo indices are left unchanged" registryindex
        =<< absindex registryindex

  , "gettable gets a table value" =:
    Just 13.37 `shouldBeResultOf` do
      pushLuaExpr @Lua.Exception "{sum = 13.37}"
      pushstring "sum"
      gettable (nth 2)
      tonumber top

  , "rawlen gives the length of a list" =:
    7 `shouldBeResultOf` do
      pushLuaExpr @Lua.Exception "{1, 1, 2, 3, 5, 8, 13}"
      rawlen top

  , testGroup "Type checking"
    [ "isfunction" ?: do
        pushLuaExpr @Lua.Exception "function () print \"hi!\" end"
        isfunction (-1)

    , "isnil" ?: pushLuaExpr @Lua.Exception "nil" *> isnil (-1)

    , "isnone" ?: isnone 5 -- stack index 5 does not exist

    , "isnoneornil" ?: do
        pushLuaExpr @Lua.Exception "nil"
        (&&) <$> isnoneornil 5 <*> isnoneornil (-1)
    ]

  , testCase "CFunction handling" . run $ do
      pushcfunction luaopen_debug
      liftIO . assertBool "not recognized as CFunction" =<< iscfunction (-1)
      liftIO . assertEqual "CFunction changed after receiving it from the stack"
        (Just luaopen_debug) =<< tocfunction (-1)

  , testGroup "getting values"
    [ testGroup "tointeger"
      [ "tointeger returns numbers verbatim" =:
        Just 149 `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "149"
          tointeger (-1)

      , "tointeger accepts strings coercible to integers" =:
        Just 451 `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "'451'"
          tointeger (-1)

      , "tointeger returns Nothing when given a boolean" =:
        Nothing `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "true"
          tointeger (-1)
      ]

    , testGroup "tonumber"
      [ "tonumber returns numbers verbatim" =:
        Just 14.9 `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "14.9"
          tonumber (-1)

      , "tonumber accepts strings as numbers" =:
        Just 42.23 `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "'42.23'"
          tonumber (-1)

      , "tonumber returns Nothing when given a boolean" =:
        Nothing `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "true"
          tonumber (-1)
      ]

    , testGroup "tostring"
      [ "get a string" =:
        Just "a string" `shouldBeResultOf` do
          pushLuaExpr @Lua.Exception "'a string'"
          tostring top

      , "get a number as string" =:
        Just "17.0" `shouldBeResultOf` do
          pushnumber 17
          tostring top

      , "fail when looking at a boolean" =:
        Nothing `shouldBeResultOf` do
          pushboolean True
          tostring top
      ]
    ]

  , "setting and getting a global works" =:
    Just "Moin" `shouldBeResultOf` do
      pushLuaExpr @Lua.Exception "{'Moin', Hello = 'World'}"
      setglobal "hamburg"

      -- get first field
      getglobal "hamburg"
      rawgeti top 1 -- first field
      tostring top

  , testGroup "get functions (Lua to stack)"
    [ "unicode characters in field name are ok" =:
      True `shouldBeResultOf` do
        pushLuaExpr @Lua.Exception "{['\xE2\x9A\x94'] = true}"
        getfield top "⚔"
        toboolean top
    ]

  , "setting and getting a global works" =:
    Just "Fisch" `shouldBeResultOf` do
      newhsuserdata ()
      pushstring "Fisch"
      setuservalue (nth 2)

      -- get uservalue again
      TypeString <- getuservalue top
      tostring top

  , "can push and receive a thread" ?: do
      luaSt <- state
      isMain <- pushthread
      liftIO (assertBool "pushing the main thread should return True" isMain)
      luaSt' <- tothread top
      return (Just luaSt == luaSt')

  , "different threads are not equal in Haskell" ?:
    liftIO
      (do luaSt1 <- newstate
          luaSt2 <- newstate
          let result = luaSt1 /= luaSt2
          close luaSt1
          close luaSt2
          return result)

  , testGroup "thread status"
    [ "OK is base thread status" =:
      OK `shouldBeResultOf` status

    , "Yield is the thread status after yielding" =:
      Yield `shouldBeResultOf` do
        openlibs
        getglobal "coroutine"
        getfield top "resume"
        pushLuaExpr @Lua.Exception "coroutine.create(function() coroutine.yield(9) end)"
        contThread <- fromMaybe (Prelude.error "not a thread at top of stack")
                      <$> tothread top
        call 1 0
        liftIO $ runWith contThread status
    ]

  , testGroup "miscellaneous functions"
    [ testGroup "pushglobaltable"
      [ "globals are fields in global table" =:
        "yep" `shouldBeResultOf` do
          pushstring "yep"
          setglobal "TEST"
          pushglobaltable
          getfield top "TEST"
          tostring' top
      ]
    ]

  , testGroup "auxiliary functions"
    [ testGroup "tostring'"
      [ "integers are converted in base10" =:
        "5" `shouldBeResultOf` do
          pushinteger 5
          tostring' top

      , "a nil value is converted into the literal string 'nil'" =:
        "nil" `shouldBeResultOf` do
          pushnil
          tostring' top

      , "strings are returned verbatim" =:
        "Hello\NULWorld" `shouldBeResultOf` do
          pushstring "Hello\NULWorld"
          tostring' top

      , "string for userdata shows the pointer value" =:
        ("userdata: " `B.isPrefixOf`) `shouldHoldForResultOf` do
          l <- state
          liftIO . Foreign.alloca $ \ptr ->
            runWith l (pushlightuserdata (ptr :: Foreign.Ptr Int))
          tostring' top

      , "string is also pushed to the stack" =:
        Just "true" `shouldBeResultOf` do
          pushboolean True
          _ <- tostring' top
          tostring top  -- note the use of tostring instead of tostring'

      , "errors during metamethod execution are caught" =:
        "'__tostring' must return a string" `shouldBeErrorMessageOf` do
          -- create a table with a faulty `__tostring` metamethod
          let mt = "{__tostring = function() return nil end }"
          let tbl = "return setmetatable({}, " `append` mt `append` ")"
          openlibs <* dostring tbl
          tostring' top
      ]

    , testGroup "ref and unref"
      [ "store nil value to registry" =:
        Lua.RefNil `shouldBeResultOf` do
          Lua.pushnil
          Lua.ref Lua.registryindex

      , "get referenced value from registry" =:
        Just "Berlin" `shouldBeResultOf` do
          Lua.pushstring "Berlin"
          cityref <- Lua.ref Lua.registryindex
          Lua.pushnil -- dummy op
          Lua.getref Lua.registryindex cityref
          Lua.tostring Lua.top

      , "references become invalid after unref" =:
        Nothing `shouldBeResultOf` do
          Lua.pushstring "Heidelberg"
          cityref <- Lua.ref Lua.registryindex
          Lua.unref Lua.registryindex cityref
          Lua.getref Lua.registryindex cityref
          Lua.tostring Lua.top
      ]
    ]

  , testGroup "loading"
    [ testGroup "loadstring"
      [ "loading a valid string should succeed" =:
        OK `shouldBeResultOf` loadstring "return 1"

      , "loading an invalid string should give a syntax error" =:
        ErrSyntax `shouldBeResultOf` loadstring "marzipan"
      ]

    , testGroup "dostring"
      [ "loading a string which fails should give a run error" =:
        ErrRun `shouldBeResultOf` dostring "error 'this fails'"

      , "loading an invalid string should return a syntax error" =:
        ErrSyntax `shouldBeResultOf` dostring "marzipan"

      , "loading a valid program should succeed" =:
        OK `shouldBeResultOf` dostring "return 1"

      , "top of the stack should be result of last computation" =:
        Just 5 `shouldBeResultOf`
          (dostring "return (2+3)" *> tointeger top)
      ]

    , testGroup "loadbuffer"
      [ "loading a valid string should succeed" =:
        OK `shouldBeResultOf` loadbuffer "return '\NUL'" "test"

      , "loading a string containing NUL should be correct" =:
        Just "\NUL" `shouldBeResultOf` do
          _ <- loadbuffer "return '\NUL'" "test"
          call 0 1
          tostring top
      ]

    , testGroup "loadfile"
      [ "file error should be returned when file does not exist" =:
        ErrFile `shouldBeResultOf` loadfile "./file-does-not-exist.lua"

      , "loading an invalid file should give a syntax error" =:
        ErrSyntax `shouldBeResultOf` loadfile "test/lua/syntax-error.lua"

      , "loading a valid program should succeed" =:
        OK `shouldBeResultOf` loadfile "./test/lua/example.lua"

      , "example fib program should be loaded correctly" =:
        Just 8 `shouldBeResultOf` do
          loadfile "./test/lua/example.lua" *> call 0 0
          getglobal "fib"
          pushinteger 6
          call 1 1
          tointeger top
      ]

    , testGroup "dofile"
      [ "file error should be returned when file does not exist" =:
        ErrFile `shouldBeResultOf` dofile "./file-does-not-exist.lua"

      , "loading an invalid file should give a syntax error" =:
        ErrSyntax `shouldBeResultOf` dofile "test/lua/syntax-error.lua"

      , "loading a failing program should give an run error" =:
        ErrRun `shouldBeResultOf` dofile "test/lua/error.lua"

      , "loading a valid program should succeed" =:
        OK `shouldBeResultOf` dofile "./test/lua/example.lua"

      , "example fib program should be loaded correctly" =:
        Just 21 `shouldBeResultOf` do
          _ <- dofile "./test/lua/example.lua"
          getglobal "fib"
          pushinteger 8
          call 1 1
          tointeger top
      ]
    ]

  , testGroup "pcall"
    [ "raising an error should lead to an error status" =:
      ErrRun `shouldBeResultOf` do
        _ <- loadstring "error \"this fails\""
        pcall 0 0 Nothing

    , "raising an error in the error handler should give a 'double error'" =:
      ErrErr `shouldBeResultOf` do
        pushLuaExpr @Lua.Exception "function () error 'error in error handler' end"
        _ <- loadstring "error \"this fails\""
        pcall 0 0 (Just (nth 2))
    ]

  , testCase "garbage collection" . run $
      -- test that gc can be called with all constructors of type GCControl.
      mapM_ gc [ GCStop, GCRestart, GCCollect, GCCollect, GCCountb
               , GCStep, GCSetPause 23, GCSetStepMul 5, GCIsRunning ]

  , testGroup "compare"
    [ testProperty "identifies strictly smaller values" $ compareWith (<) Lua.LT
    , testProperty "identifies smaller or equal values" $ compareWith (<=) Lua.LE
    , testProperty "identifies equal values" $ compareWith (==) Lua.EQ
    ]

  , testProperty "lessthan works" $ \n1 n2 -> monadicIO $ do
      luaCmp <- QCMonadic.run . run @Lua.Exception $ do
        pushnumber n2
        pushnumber n1
        lessthan (-1) (-2) <* pop 2
      assert $ luaCmp == (n1 < n2)

  , testProperty "order of Lua types is consistent" $ \ lt1 lt2 ->
      let n1 = toType lt1
          n2 = toType lt2
      in Prelude.compare n1 n2 == Prelude.compare lt1 lt2

  , testCase "boolean values are correct" $ do
      trueIsCorrect <- run $
        pushboolean True *> dostring "return true" *> rawequal (-1) (-2)
      falseIsCorrect <- run $
        pushboolean False *> dostring "return false" *> rawequal (-1) (-2)
      assertBool "LuaBool true is not equal to Lua's true" trueIsCorrect
      assertBool "LuaBool false is not equal to Lua's false" falseIsCorrect

  , testCase "functions can throw a table as error message" $ do
      let mt = "{__tostring = function (e) return e.error_code end}"
      let err = "error(setmetatable({error_code = 23}," `append` mt `append` "))"
      res <- run . try $ openbase *> loadstring err *> call 0 0
      assertEqual "wrong error message" (Left (Lua.Exception "23")) res

  , testCase "handling table errors won't leak" $ do
      let mt = "{__tostring = function (e) return e.code end}"
      let err = "error(setmetatable({code = 5}," `append` mt `append` "))"
      let luaOp = do
            openbase
            oldtop <- gettop
            _ <- try $ loadstring err *> call 0 0
            newtop <- gettop
            return (newtop - oldtop)
      res <- run @Lua.Exception luaOp
      assertEqual "error handling leaks values to the stack" 0 res

  , HsLua.Core.PrimaryTests.tests
  , HsLua.Core.ClosuresTests.tests
  , HsLua.Core.PackageTests.tests
  , HsLua.Core.RunTests.tests
  , HsLua.Core.TraceTests.tests
  , HsLua.Core.UnsafeTests.tests
  , HsLua.Core.UserdataTests.tests
  ]

compareWith :: (Lua.Integer -> Lua.Integer -> Bool)
            -> RelationalOperator -> Lua.Integer -> Property
compareWith op luaOp n = compareLT .&&. compareEQ .&&. compareGT
 where
  compareLT :: Property
  compareLT = monadicIO  $ do
    luaCmp <- QCMonadic.run . run $ do
      pushinteger $ n - 1
      pushinteger n
      compare @Lua.Exception (-2) (-1) luaOp
    assert $ luaCmp == op (n - 1) n

  compareEQ :: Property
  compareEQ = monadicIO  $ do
    luaCmp <- QCMonadic.run . run $ do
      pushinteger n
      pushinteger n
      compare @Lua.Exception (-2) (-1) luaOp
    assert $ luaCmp == op n n

  compareGT :: Property
  compareGT = monadicIO $ do
    luaRes <- QCMonadic.run . run $ do
      pushinteger $ n + 1
      pushinteger n
      compare @Lua.Exception (-2) (-1) luaOp
    assert $ luaRes == op (n + 1) n
