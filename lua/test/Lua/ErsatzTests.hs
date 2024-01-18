{-|
Module      : Lua.ErsatzTests
Copyright   : © 2021-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta

Tests for Lua bindings.
-}
module Lua.ErsatzTests (tests) where

import Control.Monad (void)
import Foreign.C.String (withCString, withCStringLen)
import Foreign.Marshal (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Lua
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@=?) )

-- | Tests for unsafe methods.
tests :: TestTree
tests = testGroup "ersatz"
  [ testGroup "arith"
    [ "adds two numbers" =: do
        7 `shouldBeResultOf` \l -> do
          lua_pushinteger l 5
          lua_pushinteger l 2
          hslua_arith l LUA_OPADD nullPtr
          lua_tointegerx l top nullPtr
    , "negates number" =: do
        (-5) `shouldBeResultOf` \l -> do
          lua_pushinteger l 5
          hslua_arith l LUA_OPUNM nullPtr
          lua_tointegerx l top nullPtr
    , "pops its arguments from the stack" =: do
        1 `shouldBeResultOf` \l -> do
          old <- lua_gettop l
          lua_pushinteger l 4
          lua_pushinteger l 3
          hslua_arith l LUA_OPADD nullPtr
          new <- lua_gettop l
          return (new - old)
    , "pops a single argument for unary negation" =: do
        1 `shouldBeResultOf` \l -> do
          old <- lua_gettop l
          lua_pushinteger l 3
          hslua_arith l LUA_OPUNM nullPtr
          new <- lua_gettop l
          return (new - old)
    , "sets status to LUA_OK on success" =: do
        (LUA_OK, 1024) `shouldBeResultOf` \l -> do
          lua_pushinteger l 2
          lua_pushinteger l 10
          stts <- alloca $ \status -> do
            hslua_arith l LUA_OPPOW status
            peek status
          result <- lua_tointegerx l top nullPtr
          return (stts, result)
    , "sets error status on error" =: do
        LUA_ERRRUN `shouldBeResultOf` \l -> do
          lua_pushinteger l 2
          lua_pushboolean l TRUE
          alloca $ \status -> do
            hslua_arith l LUA_OPSHR status
            peek status
    , "runs metamethod" =: do
        (LUA_OK, 7) `shouldBeResultOf` \l -> do
          lua_pushinteger l 2
          lua_createtable l 0 0
          LUA_OK <- withCStringLen "return {__mod = function() return 7 end}" $
            \(s, len) -> luaL_loadbuffer l s (fromIntegral len) s
          LUA_OK <- lua_pcall l 0 1 0
          lua_setmetatable l (-2)
          stts <- alloca $ \status -> do
            hslua_arith l LUA_OPMOD status
            peek status
          result <- lua_tointegerx l top nullPtr
          return (stts, result)
    , "catches runtime error in metamethod" =: do
        LUA_ERRRUN `shouldBeResultOf` \l -> do
          lua_pushinteger l 2
          lua_createtable l 0 0
          LUA_OK <- withCStringLen "return {__bor = function() error'no' end}" $
            \(s, len) -> luaL_loadbuffer l s (fromIntegral len) s
          LUA_OK <- lua_pcall l 0 1 0
          lua_setmetatable l (-2)
          alloca $ \status -> do
            hslua_arith l LUA_OPBOR status
            peek status
    ]
  , testGroup "global"
    [ "set and get global field" =: do
        (42, LUA_TNUMBER) `shouldBeResultOf` \l -> do
          lua_pushinteger l 42
          tp <- withCStringLen "ups" $ \(name, nameLen) -> do
            hslua_setglobal l name (fromIntegral nameLen) nullPtr
            hslua_getglobal l name (fromIntegral nameLen) nullPtr
          i  <- lua_tointegerx l top nullPtr
          return (i, tp)

    , "get value from table" =: do
        (-23, LUA_TNUMBER) `shouldBeResultOf` \l -> do
          withCStringLen "return {[5] = -23}" $ \(name, len) -> void $
            luaL_loadbuffer l name (fromIntegral len) name
          LUA_OK <- lua_pcall l 0 1 0
          lua_pushinteger l 5
          ty <- hslua_gettable l (nth 2) nullPtr
          i  <- lua_tointegerx l top nullPtr
          return (i, ty)
    ]

  , testGroup "Auxiliary"
    [ testGroup "hslua_requiref"
      [ "can load a module" =: do
        LUA_TTABLE `shouldBeResultOf` \l -> do
          lua_pushcfunction l luaopen_package
          LUA_OK <- lua_pcall l 0 0 0
          withCString "math" $ \s ->
            hsluaL_requiref l s luaopen_math FALSE nullPtr
          lua_type l (-1)

      , "returns LUA_OK on success" =: do
        LUA_OK `shouldBeResultOf` \l -> do
          lua_pushcfunction l luaopen_package
          LUA_OK <- lua_pcall l 0 0 0
          alloca $ \status -> do
            withCString "math" $ \s ->
              hsluaL_requiref l s luaopen_package FALSE status
            peek status

      , "sets global if flag is set" =: do
        LUA_TTABLE `shouldBeResultOf` \l -> do
          luaL_openlibs l
          withCString "mathematics" $ \s ->
            hsluaL_requiref l s luaopen_math TRUE nullPtr
          withCStringLen "mathematics" $ \(name, len) ->
            hslua_getglobal l name (fromIntegral len) nullPtr

      , "catches errors" =: do
        LUA_ERRRUN `shouldBeResultOf` \l -> do
          -- unset registry
          lua_pushnil l
          lua_copy l (-1) LUA_REGISTRYINDEX
          alloca $ \status -> do
            withCString "math" $ \s ->
              hsluaL_requiref l s luaopen_package FALSE status
            peek status
      ]
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
