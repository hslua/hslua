{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
Module      : Lua.UnsafeTests
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Tests for bindings to unsafe functions.
-}
module Lua.UnsafeTests (tests) where

import Foreign.C.String (withCString, withCStringLen)
import Foreign.Ptr (nullPtr)
import Lua
import Lua.Primary
  ( lua_getglobal, lua_gettable, lua_next, lua_setglobal, lua_settable )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@=?) )

-- | Tests for unsafe methods.
tests :: TestTree
tests = testGroup "Unsafe"
  [ testGroup "tables"
    [ "set and get integer field" =: do
        (-23, LUA_TNUMBER) `shouldBeResultOf` \l -> do
          lua_createtable l 0 0
          lua_pushinteger l 5
          lua_pushinteger l (-23)
          lua_settable l (nth 3)
          lua_pushinteger l 5
          tp <- lua_gettable l (nth 2)
          i  <- lua_tointegerx l top nullPtr
          return (i, tp)

    , "get metamethod field" =: do
        (TRUE, LUA_TBOOLEAN) `shouldBeResultOf` \l -> do
          -- create table
          lua_createtable l 0 0
          -- create metatable
          lua_createtable l 0 0
          withCStringLen "__index" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          -- create index table
          lua_createtable l 0 0
          lua_pushinteger l 5
          lua_pushboolean l TRUE
          lua_rawset l (nth 3)
          -- set index table to "__index" in metatable
          lua_rawset l (nth 3)
          -- set metatable
          lua_setmetatable l (nth 2)
          -- access field in metatable
          lua_pushinteger l 5
          tp <- lua_gettable l (nth 2)
          b  <- lua_toboolean l top
          return (b, tp)

    , "set metamethod field" =: do
        1337 `shouldBeResultOf` \l -> do
          lua_createtable l 0 0     -- index table
          -- create table t
          lua_createtable l 0 0
          -- create metatable
          lua_createtable l 0 0
          withCStringLen "__newindex" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          lua_pushvalue l (nth 4)   -- index table
          -- set index table to "__newindex" in metatable
          lua_rawset l (nth 3)
          -- set metatable
          lua_setmetatable l (nth 2)

          -- set field n index table via __newindex on t
          lua_pushinteger l 1
          lua_pushinteger l 1337
          lua_settable l (nth 3)

          lua_pop l 1               -- drop table t
          lua_pushinteger l 1
          lua_rawget l (nth 2)
          lua_tointegerx l top nullPtr
    ]

  , testGroup "globals"
    [ "get global from base library" =:
      LUA_TFUNCTION `shouldBeResultOf` \l -> do
        luaL_openlibs l
        withCString "print" $ \ptr ->
          lua_getglobal l ptr

    , "set global" =:
      13.37 `shouldBeResultOf` \l -> do
        lua_pushnumber l 13.37
        withCString "foo" $ lua_setglobal l
        lua_pushglobaltable l
        withCStringLen "foo" $ \(ptr, len) ->
          lua_pushlstring l ptr (fromIntegral len)
        lua_rawget l (nth 2)
        lua_tonumberx l top nullPtr
    ]

  , testGroup "next"
    [ "get next key from table" =:
      41 `shouldBeResultOf` \l -> do
        -- create table {41}
        lua_createtable l 0 0
        lua_pushinteger l 41
        lua_rawseti l (nth 2) 1
        -- first key
        lua_pushnil l
        x <- lua_next l (nth 2)
        if x == FALSE
          then fail "expected truish return value"
          else lua_tonumberx l top nullPtr

    , "returns FALSE if table is empty" =:
      FALSE `shouldBeResultOf` \l -> do
        lua_createtable l 0 0
        lua_pushnil l
        lua_next l (nth 2)
    ]
  ]

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase

shouldBeResultOf :: (HasCallStack, Eq a, Show a)
                 => a -> (State -> IO a) -> Assertion
shouldBeResultOf expected luaOp = do
  result <- withNewState luaOp
  expected @=? result
