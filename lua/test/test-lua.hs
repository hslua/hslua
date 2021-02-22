{-# LANGUAGE CPP #-}
{-|
Module      : Main
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Tests for the raw Lua bindings.
-}
module Main (main) where

#ifdef ALLOW_UNSAFE_GC
import Control.Monad (void)
#else
import Control.Monad (forM_, void)
import Data.IORef (newIORef, readIORef, writeIORef)
#endif

import Foreign.C.String (peekCString, withCStringLen)
import Foreign.Marshal (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable as Storable
import Foreign.Lua
import Foreign.Lua.Call
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion, assertBool, testCase, (@=?) )

-- | Runs tests.
main :: IO ()
main = defaultMain tests

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua"
  [ testGroup "thread"
    [ "create and close" =: do
      l <- hsluaL_newstate
      lua_close l

    , "newthread" =: do
        (5, 23) `shouldBeResultOf` \l -> do
          l1 <- lua_newthread l
          lua_pushnumber l 5
          lua_pushnumber l1 23
          (,) <$> lua_tonumberx l  top nullPtr
              <*> lua_tonumberx l1 top nullPtr

    , "type check" =: do
        TRUE `shouldBeResultOf` \l -> do
          _ <- lua_newthread l
          lua_isthread l top

    , "thread type is LUA_TTHREAD" =: do
        LUA_TTHREAD `shouldBeResultOf` \l -> do
          _ <- lua_newthread l
          lua_type l top

    , "pushing" =: do
        LUA_TTHREAD `shouldBeResultOf` \l -> do
          newl <- lua_newthread l
          lua_pop l 1
          _ <- lua_pushthread newl
          lua_type newl top
    ]

  , testGroup "booleans"
    [ "push and retrieve" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_pushboolean l TRUE
          lua_toboolean l top

    , "check" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_pushboolean l FALSE
          lua_isboolean l top

    , "type" =: do
        LUA_TBOOLEAN `shouldBeResultOf` \l -> do
          lua_pushboolean l FALSE
          lua_type l top
    ]

  , testGroup "numbers"
    [ "push and retrieve integer" =: do
        5 `shouldBeResultOf` \l -> do
          lua_pushinteger l 5
          lua_tointegerx l top nullPtr

    , "push and retrieve float" =: do
        (-0.1) `shouldBeResultOf` \l -> do
          lua_pushnumber l (-0.1)
          lua_tonumberx l top nullPtr

    , "check for integer" =: do
        (TRUE, FALSE) `shouldBeResultOf` \l -> do
          lua_pushinteger l 0
          t <- lua_isinteger l top
          lua_pushnil l
          f <- lua_isinteger l top
          return (t, f)

    , "check for number" =: do
        (TRUE, FALSE) `shouldBeResultOf` \l -> do
          lua_pushinteger l 0
          t <- lua_isnumber l top
          lua_pushnil l
          f <- lua_isnumber l top
          return (t, f)

    , "type" =: do
        LUA_TNUMBER `shouldBeResultOf` \l -> do
          lua_pushinteger l 0
          lua_type l top
    ]

  , testGroup "nil"
    [ "push and type check" =:
      (TRUE, FALSE) `shouldBeResultOf` \l -> do
        lua_pushnil l
        t <- lua_isnil l top
        lua_pushglobaltable l
        f <- lua_isnil l top
        return (t, f)

    , "type is LUA_TNIL" =:
      LUA_TNIL `shouldBeResultOf` \l -> do
        lua_pushnil l
        lua_type l top
    ]

  , testGroup "none"
    [ "invalid index is 'none'" =:
      TRUE `shouldBeResultOf` \l -> lua_isnone l 1

    , "valid index is not none" =:
      FALSE `shouldBeResultOf` \l -> do
        lua_pushnil l
        lua_isnone l 1

    , "invalid index has type LUA_TNONE" =:
      LUA_TNONE `shouldBeResultOf` \l -> lua_type l 9
    ]

  , testGroup "strings"
    [ "push and retrieve" =: do
        "testing" `shouldBeResultOf` \l -> do
          withCStringLen "testing" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          peekCString =<< lua_tolstring l top nullPtr

    , "type" =: do
        LUA_TSTRING `shouldBeResultOf` \l -> do
          withCStringLen "Olsen Olsen" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          lua_type l top
    ]

  , testGroup "tables"
    [ "check type" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_createtable l 0 0
          lua_istable l top

    , "has type LUA_TTABLE" =: do
        LUA_TTABLE `shouldBeResultOf` \l -> do
          lua_createtable l 0 0
          lua_type l top
    ]

  , testGroup "constants"
    [ "loadedTableRegistryField"  =:
      ("_LOADED"  @=? loadedTableRegistryField)
    , "preloadTableRegistryField" =:
      ("_PRELOAD" @=? preloadTableRegistryField)
    ]

  , testGroup "compare"
    [ "equality" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_pushinteger l 42
          lua_pushnumber l 42
          hslua_compare l (nth 2) (nth 1) LUA_OPEQ nullPtr

    , "less then" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_pushinteger l (-2)
          lua_pushnumber l 3
          hslua_compare l (nth 2) (nth 1) LUA_OPLT nullPtr

    , "not less then" =: do
        FALSE `shouldBeResultOf` \l -> do
          lua_pushinteger l 42
          lua_pushnumber l 42
          hslua_compare l (nth 2) (nth 1) LUA_OPLT nullPtr

    , "less then or equal" =: do
        TRUE `shouldBeResultOf` \l -> do
          lua_pushinteger l 23
          lua_pushnumber l 42
          alloca $ \statusPtr -> do
            result <- hslua_compare l (nth 2) (nth 1) LUA_OPLE statusPtr
            status <- Storable.peek statusPtr
            assertBool "comparison failed" (LUA_OK == status)
            return result
    ]

  , testGroup "function calling"
    [ "call `type(true)`" =: do
        "boolean" `shouldBeResultOf` \l -> do
          luaL_openlibs l
          -- push function `type`
          lua_pushglobaltable l
          withCStringLen "type" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          lua_rawget l (nth 2)
          -- push boolean
          lua_pushboolean l TRUE
          status <- lua_pcall l (NumArgs 1) (NumResults 1) 0
          assertBool "call status" (status == LUA_OK)
          peekCString =<< lua_tolstring l top nullPtr

    , "call type" =: do
        (== LUA_ERRRUN) `shouldHoldForResultOf` \l -> do
          luaL_openlibs l
          -- push function `error`
          lua_pushglobaltable l
          withCStringLen "error" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          lua_rawget l (nth 2)
          -- push boolean
          lua_pushboolean l TRUE
          lua_pcall l (NumArgs 1) (NumResults 1) 0
    ]

  , testGroup "garbage-collection"
    [ "stop, restart GC"  =: do
        counts <- withNewState $ \l -> do
          lua_createtable l 0 0
          _  <- lua_gc l LUA_GCSTOP 0
          lua_pop l 1
          kb1 <- lua_gc l LUA_GCCOUNT 0
          b1  <- lua_gc l LUA_GCCOUNTB 0
          _   <- lua_gc l LUA_GCCOLLECT 0
          kb2 <- lua_gc l LUA_GCCOUNT 0
          b2  <- lua_gc l LUA_GCCOUNTB 0
          return (b1 + 1024 * kb1, b2 + 1024 * kb2)
        assertBool "first count should be larger" (uncurry (>) counts)
    , "count memory" =: do
        count <- withNewState $ \l -> do
          lua_gc l LUA_GCCOUNT 0
        assertBool "memory consumption not between 0 and 10 kB"
                   (count > 0 && count < 10)
    ]

  , testGroup "Haskell functions"
    [ let add5 l = do
            n <- lua_tointegerx l top nullPtr
            lua_pushinteger l $ n + 5
            return (NumResults 1)
      in "call Haskell function" =: do
        23 `shouldBeResultOf` \l -> do
          hslua_pushhsfunction l add5
          lua_pushinteger l 18
          void $ lua_pcall l (NumArgs 1) (NumResults 1) 0
          lua_tointegerx l (nth 1) nullPtr

#ifndef ALLOW_UNSAFE_GC
    , "Haskell function as finalizer" =: do
        msg <- newIORef "nope"
        let sendMessage _ = do
              writeIORef msg "HI MOM!"
              return (NumResults 0)
        "HI MOM!" `shouldBeResultOf` \l -> do
          -- create dummy table
          lua_createtable l 0 0
          -- create metatable with Haskell __gc function
          lua_createtable l 0 0
          withCStringLen "__gc" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          hslua_pushhsfunction l sendMessage
          lua_rawset l (nth 3)
          -- set metatable with finalizer
          lua_setmetatable l (nth 2)
          -- remove dummy table from stack so the GC to collect it
          lua_pop l 1
          -- perform a large number of operations to allow the GC to kick in.
          forM_ [1..100] $ \i -> do
            -- push some string
            withCStringLen "some nonesense" $ \(ptr, len) ->
              lua_pushlstring l ptr (fromIntegral len)
            -- create new table with integer field
            lua_createtable l 0 0
            lua_pushinteger l i
            lua_pushinteger l 23
            lua_rawset l (nth 3)
            -- set empty table as metatable
            lua_createtable l 0 0
            lua_setmetatable l (nth 2)
            -- remove table and strings from stack
            lua_pop l 2
          -- the GC should have run now, check the message
          readIORef msg
#endif
    ]
  ]

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase

shouldBeResultOf :: (Eq a, Show a) => a -> (State -> IO a) -> Assertion
shouldBeResultOf expected luaOp = do
  result <- withNewState luaOp
  expected @=? result

shouldHoldForResultOf :: (a -> Bool) -> (State -> IO a) -> Assertion
shouldHoldForResultOf predicate luaOp = do
  result <- withNewState luaOp
  assertBool "predicate does not hold" (predicate result)
