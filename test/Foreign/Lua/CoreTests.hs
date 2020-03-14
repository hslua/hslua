{-
Copyright © 2017-2020 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  Foreign.Lua.CoreTests
Copyright   :  © 2017-2020 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for Lua C API-like functions.
-}
module Foreign.Lua.CoreTests (tests) where

import Prelude hiding (compare)

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Foreign.Lua as Lua
import Test.HsLua.Arbitrary ()
import Test.HsLua.Util ( (?:), (=:), shouldBeErrorMessageOf, shouldBeResultOf
                       , shouldHoldForResultOf, pushLuaExpr )
import Test.QuickCheck (Property, (.&&.))
import Test.QuickCheck.Monadic (assert, monadicIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Prelude
import qualified Data.ByteString as B
import qualified Foreign.Lua.Core.RawBindings as LuaRaw
import qualified Foreign.Lua.Core.AuxiliaryTests
import qualified Foreign.Lua.Core.ErrorTests
import qualified Foreign.Marshal as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Test.QuickCheck.Monadic as QCMonadic


-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Core module"
  [ Foreign.Lua.Core.ErrorTests.tests
  , Foreign.Lua.Core.AuxiliaryTests.tests
  , testGroup "copy"
    [ "copies stack elements using positive indices" ?: do
        pushLuaExpr "5, 4, 3, 2, 1"
        copy 4 3
        rawequal (nthFromBottom 4) (nthFromBottom 3)

    , "copies stack elements using negative indices" ?: do
        pushLuaExpr "5, 4, 3, 2, 1"
        copy (-1) (-3)
        rawequal (-1) (-3)
    ]

  , testGroup "insert"
    [ "inserts stack elements using negative indices" ?: do
        pushLuaExpr "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert (-6)
        movedEl <- peek (-6) :: Lua Lua.Integer
        newTop <- peek (-1) :: Lua Lua.Integer
        return (movedEl == 9 && newTop == 8)

    , "inserts stack elements using negative indices" ?: do
        pushLuaExpr "1, 2, 3, 4, 5, 6, 7, 8, 9"
        insert 4
        movedEl <- peek 4 :: Lua Lua.Integer
        newTop <- peek (-1) :: Lua Lua.Integer
        return (movedEl == 9 && newTop == 8)
    ]

  , testCase "absindex" . run $ do
      pushLuaExpr "1, 2, 3, 4"
      liftIO . assertEqual "index from bottom doesn't change" (nthFromBottom 3)
        =<< absindex (nthFromBottom 3)
      liftIO . assertEqual "index from top is made absolute" (nthFromBottom 2)
        =<< absindex (nthFromTop 3)
      liftIO . assertEqual "pseudo indices are left unchanged" registryindex
        =<< absindex registryindex

  , "gettable gets a table value" =:
    Just 13.37 `shouldBeResultOf` do
      pushLuaExpr "{sum = 13.37}"
      pushstring "sum"
      gettable (nthFromTop 2)
      tonumber stackTop

  , "rawlen gives the length of a list" =:
    7 `shouldBeResultOf` do
      pushLuaExpr "{1, 1, 2, 3, 5, 8, 13}"
      rawlen stackTop

  , testGroup "Type checking"
    [ "isfunction" ?: do
        pushLuaExpr "function () print \"hi!\" end"
        isfunction (-1)

    , "isnil" ?: pushLuaExpr "nil" *> isnil (-1)

    , "isnone" ?: isnone 500 -- stack index 500 does not exist

    , "isnoneornil" ?: do
        pushLuaExpr "nil"
        (&&) <$> isnoneornil 500 <*> isnoneornil (-1)
    ]

  , testCase "CFunction handling" . run $ do
      pushcfunction LuaRaw.lua_open_debug_ptr
      liftIO . assertBool "not recognized as CFunction" =<< iscfunction (-1)
      liftIO . assertEqual "CFunction changed after receiving it from the stack"
        (Just LuaRaw.lua_open_debug_ptr) =<< tocfunction (-1)

  , testGroup "getting values"
    [ testGroup "tointeger"
      [ "tointeger returns numbers verbatim" =:
        Just 149 `shouldBeResultOf` do
          pushLuaExpr "149"
          tointeger (-1)

      , "tointeger accepts strings coercible to integers" =:
        Just 451 `shouldBeResultOf` do
          pushLuaExpr "'451'"
          tointeger (-1)

      , "tointeger returns Nothing when given a boolean" =:
        Nothing `shouldBeResultOf` do
          pushLuaExpr "true"
          tointeger (-1)
      ]

    , testGroup "tonumber"
      [ "tonumber returns numbers verbatim" =:
        Just 14.9 `shouldBeResultOf` do
          pushLuaExpr "14.9"
          tonumber (-1)

      , "tonumber accepts strings as numbers" =:
        Just 42.23 `shouldBeResultOf` do
          pushLuaExpr "'42.23'"
          tonumber (-1)

      , "tonumber returns Nothing when given a boolean" =:
        Nothing `shouldBeResultOf` do
          pushLuaExpr "true"
          tonumber (-1)
      ]

    , testGroup "tostring"
      [ "get a string" =:
        Just "a string" `shouldBeResultOf` do
          pushLuaExpr "'a string'"
          tostring stackTop

      , "get a number as string" =:
        Just "17.0" `shouldBeResultOf` do
          pushnumber 17
          tostring stackTop

      , "fail when looking at a boolean" =:
        Nothing `shouldBeResultOf` do
          pushboolean True
          tostring stackTop
      ]
    ]

  , "setting and getting a global works" =:
    Just "Moin" `shouldBeResultOf` do
      pushLuaExpr "{'Moin', Hello = 'World'}"
      setglobal "hamburg"

      -- get first field
      getglobal "hamburg"
      rawgeti stackTop 1 -- first field
      tostring stackTop

  , testGroup "get functions (Lua to stack)"
    [ "unicode characters in field name are ok" =:
      True `shouldBeResultOf` do
        pushLuaExpr "{['\xE2\x9A\x94'] = true}"
        getfield stackTop "⚔"
        toboolean stackTop
    ]

  , "can push and receive a thread" ?: do
      luaSt <- state
      isMain <- pushthread
      liftIO (assertBool "pushing the main thread should return True" isMain)
      luaSt' <- peek stackTop
      return (luaSt == luaSt')

  , "different threads are not equal in Haskell" ?: do
      luaSt1 <- liftIO newstate
      luaSt2 <- liftIO newstate
      return (luaSt1 /= luaSt2)

  , testGroup "thread status"
    [ "OK is base thread status" =:
      OK `shouldBeResultOf` status

    , "Yield is the thread status after yielding" =:
      Yield `shouldBeResultOf` do
        openlibs
        getglobal "coroutine"
        getfield stackTop "resume"
        pushLuaExpr "coroutine.create(function() coroutine.yield(9) end)"
        contThread <- fromMaybe (Prelude.error "not a thread at top of stack")
                      <$> tothread stackTop
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
          getfield stackTop "TEST"
          tostring' stackTop
      ]
    ]

  , testGroup "auxiliary functions"
    [ testGroup "tostring'"
      [ "integers are converted in base10" =:
        "5" `shouldBeResultOf` do
          pushinteger 5
          tostring' stackTop

      , "a nil value is converted into the literal string 'nil'" =:
        "nil" `shouldBeResultOf` do
          pushnil
          tostring' stackTop

      , "strings are returned verbatim" =:
        "Hello\NULWorld" `shouldBeResultOf` do
          pushstring "Hello\NULWorld"
          tostring' stackTop

      , "string for userdata shows the pointer value" =:
        ("userdata: " `B.isPrefixOf`) `shouldHoldForResultOf` do
          l <- state
          liftIO . Foreign.alloca $ \ptr ->
            runWith l (pushlightuserdata (ptr :: Foreign.Ptr Int))
          tostring' stackTop

      , "string is also pushed to the stack" =:
        Just "true" `shouldBeResultOf` do
          pushboolean True
          _ <- tostring' stackTop
          tostring stackTop  -- note the use of tostring instead of tostring'

      , "errors during metamethod execution are caught" =:
        "'__tostring' must return a string" `shouldBeErrorMessageOf` do
          -- create a table with a faulty `__tostring` metamethod
          let mt = "{__tostring = function() return nil end }"
          let tbl = "return setmetatable({}, " <> mt <> ")"
          openlibs <* dostring tbl
          tostring' stackTop
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
          Lua.tostring Lua.stackTop

      , "references become invalid after unref" =:
        Nothing `shouldBeResultOf` do
          Lua.pushstring "Heidelberg"
          cityref <- Lua.ref Lua.registryindex
          Lua.unref Lua.registryindex cityref
          Lua.getref Lua.registryindex cityref
          Lua.tostring Lua.stackTop
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
        (5 :: Lua.Integer) `shouldBeResultOf`
          (dostring "return (2+3)" *> peek (-1))
      ]

    , testGroup "loadbuffer"
      [ "loading a valid string should succeed" =:
        OK `shouldBeResultOf` loadbuffer "return '\NUL'" "test"

      , "loading a string containing NUL should be correct" =:
        Just "\NUL" `shouldBeResultOf` do
          _ <- loadbuffer "return '\NUL'" "test"
          call 0 1
          tostring stackTop
      ]

    , testGroup "loadfile"
      [ "file error should be returned when file does not exist" =:
        ErrFile `shouldBeResultOf` loadfile "./file-does-not-exist.lua"

      , "loading an invalid file should give a syntax error" =:
        ErrSyntax `shouldBeResultOf` loadfile "test/lua/syntax-error.lua"

      , "loading a valid program should succeed" =:
        OK `shouldBeResultOf` loadfile "./test/lua/example.lua"

      , "example fib program should be loaded correctly" =:
        (8 :: Lua.Integer) `shouldBeResultOf` do
          loadfile "./test/lua/example.lua" *> call 0 0
          getglobal "fib"
          pushinteger 6
          call 1 1
          peek stackTop
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
        (21 :: Lua.Integer) `shouldBeResultOf` do
          _ <- dofile "./test/lua/example.lua"
          getglobal "fib"
          pushinteger 8
          call 1 1
          peek stackTop
      ]
    ]

  , testGroup "pcall"
    [ "raising an error should lead to an error status" =:
      ErrRun `shouldBeResultOf` do
        _ <- loadstring "error \"this fails\""
        pcall 0 0 Nothing

    , "raising an error in the error handler should give a 'double error'" =:
      ErrErr `shouldBeResultOf` do
        pushLuaExpr "function () error 'error in error handler' end"
        _ <- loadstring "error \"this fails\""
        pcall 0 0 (Just (nthFromTop 2))
    ]

  , testCase "garbage collection" . run $
      -- test that gc can be called with all constructors of type GCCONTROL.
      forM_ [GCSTOP .. GCSETSTEPMUL] $ \what -> gc what 23

  , testGroup "compare"
    [ testProperty "identifies strictly smaller values" $ compareWith (<) Lua.LT
    , testProperty "identifies smaller or equal values" $ compareWith (<=) Lua.LE
    , testProperty "identifies equal values" $ compareWith (==) Lua.EQ
    ]

  , testProperty "lessthan works" $ \n1 n2 -> monadicIO $ do
      luaCmp <- QCMonadic.run . run $ do
        push (n2 :: Lua.Number)
        push (n1 :: Lua.Number)
        lessthan (-1) (-2) <* pop 2
      assert $ luaCmp == (n1 < n2)

  , testProperty "order of Lua types is consistent" $ \ lt1 lt2 ->
      let n1 = fromType lt1
          n2 = fromType lt2
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
      let err = "error(setmetatable({error_code = 23}," <> mt <> "))"
      res <- run . try $ openbase *> loadstring err *> call 0 0
      assertEqual "wrong error message" (Left (Lua.Exception "23")) res

  , testCase "handling table errors won't leak" $ do
      let mt = "{__tostring = function (e) return e.code end}"
      let err = "error(setmetatable({code = 5}," <> mt <> "))"
      let luaOp = do
            openbase
            oldtop <- gettop
            _ <- try $ loadstring err *> call 0 0
            newtop <- gettop
            return (newtop - oldtop)
      res <- run luaOp
      assertEqual "error handling leaks values to the stack" 0 res
  ]

compareWith :: (Lua.Integer -> Lua.Integer -> Bool)
            -> RelationalOperator -> Lua.Integer -> Property
compareWith op luaOp n = compareLT .&&. compareEQ .&&. compareGT
 where
  compareLT :: Property
  compareLT = monadicIO  $ do
    luaCmp <- QCMonadic.run . run $ do
      push $ n - 1
      push n
      compare (-2) (-1) luaOp
    assert $ luaCmp == op (n - 1) n

  compareEQ :: Property
  compareEQ = monadicIO  $ do
    luaCmp <- QCMonadic.run . run $ do
      push n
      push n
      compare (-2) (-1) luaOp
    assert $ luaCmp == op n n

  compareGT :: Property
  compareGT = monadicIO $ do
    luaRes <- QCMonadic.run . run $ do
      push $ n + 1
      push n
      compare (-2) (-1) luaOp
    assert $ luaRes == op (n + 1) n
