{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
Module      : Main
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tests for the @lpeg@ Lua package.
-}
module Main where

import Control.Monad
import Foreign.C
import Foreign.Ptr (nullPtr)
import Lua
import Lua.LPeg
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LPeg" $
  -- tests to check the correct types to make sure placeholder and the real
  -- thing don't diverge.
  [ testCase "can push lpeg loader" $ do
      l <- hsluaL_newstate
      lua_pushcclosure l luaopen_lpeg_ptr 0
      assertEqual "loader should be a function"
        LUA_TFUNCTION =<< lua_type l (-1)
      lua_close l
  , testCase "can push re loader" $ do
      l <- hsluaL_newstate
      lua_pushcclosure l luaopen_re_ptr 0
      assertEqual "loader should be a function"
        LUA_TFUNCTION =<< lua_type l (-1)
      lua_close l
  , testCase "can push searcher" $ do
      l <- hsluaL_newstate
      hslua_pushhsfunction l lpeg_searcher
      assertEqual "searcher should be a function"
        LUA_TFUNCTION =<< lua_type l (-1)
      lua_close l
  ] ++
#ifndef RELY_ON_SYSTEM_INSTALL
  [ testCase "load lpeg library via CFunction" $ do
      l <- hsluaL_newstate
      lua_pushcclosure l luaopen_lpeg_ptr 0
      stts <- lua_pcall l 0 1 0
      when (stts /= LUA_OK) $
        fail =<< peekCString =<< lua_tolstring l (-1) nullPtr

      _ <- withCString "version" $ lua_pushstring l
      lua_rawget l (-2)
      assertEqual "module should have `version` field of type function"
        LUA_TFUNCTION =<< lua_type l (-1)
      lua_close l

  , testCase "load libraries manually" $ do
      l <- hsluaL_newstate
      luaL_openlibs l

      -- unset searchers
      _ <- withCString "package" $ lua_getglobal l
      pushstring l "searchers"
      _ <- lua_gettable l (-2)
      forM_ [1..4] $ \i -> lua_pushnil l *> lua_rawseti l (-2) i
      lua_pop l 2

      -- ensure that lpeg cannot be found
      runScript l "assert(not pcall(function() require 'lpeg' end))"
      -- ensure that re cannot be found
      runScript l "assert(not pcall(function() require 're' end))"

      -- get table "_LOADED" from registry
      _ <- withCString loadedTableRegistryField $ lua_pushstring l
      lua_rawget l LUA_REGISTRYINDEX

      -- load lpeg
      _ <- withCString "lpeg" $ lua_pushstring l  -- key
      lua_pushcclosure l luaopen_lpeg_ptr 0
      stts <- lua_pcall l 0 1 0
      if stts == LUA_OK
        then lua_rawset l (-3)
        else fail =<< peekCString =<< lua_tolstring l (-1) nullPtr

      -- load re
      _ <- withCString "re" $ lua_pushstring l  -- key
      lua_pushcclosure l luaopen_re_ptr 0
      stts' <- lua_pcall l 0 1 0
      if stts' == LUA_OK
        then lua_rawset l (-3)
        else fail =<< peekCString =<< lua_tolstring l (-1) nullPtr

      runScript l testScript
      runScript l "re = require 're'; assert(type(re.gsub) == 'function')"

      lua_close l

  , testCase "use lpeg_searcher as searcher" $ do
      l <- hsluaL_newstate
      luaL_openlibs l
      _ <- withCString "package" $ lua_getglobal l
      pushstring l "searchers"
      _ <- lua_gettable l (-2)
      hslua_pushhsfunction l lpeg_searcher
      lua_rawseti l (-2) 2
      -- remove other searchers
      lua_pushnil l *> lua_rawseti l (-2) 3
      lua_pushnil l *> lua_rawseti l (-2) 4
      lua_pop l 2

      runScript l testScript
      runScript l "local re = require 're'\n"

      lua_close l
  ]
 where
  runScript l script = do
    stts <- withCStringLen script $ \(s, slen) ->
      withCString "test script" $ \n ->
      luaL_loadbuffer l s (fromIntegral slen) n
    when (stts /= LUA_OK) $ do
      fail =<< peekCString =<< lua_tolstring l (-1) nullPtr
    stts' <- lua_pcall l 0 0 0
    when (stts' /= LUA_OK) $ do
      fail =<< peekCString =<< lua_tolstring l (-1) nullPtr

testScript :: String
testScript = unlines
  [ "local m = require 'lpeg'"
  , "assert(type(m.version()) == 'string')"
  , "assert(m.type(m.P'alo') == 'pattern')"
  , "assert(m.match('a' * m.P(true), 'a') == 2)"
  ]

#else
  [ testCase "loading the lpeg placeholder library causes an error" $ do
      l <- hsluaL_newstate
      lua_pushcclosure l luaopen_lpeg_ptr 0
      stts <- lua_pcall l 0 1 0
      stts `seq` lua_close l
      when (stts /= LUA_ERRRUN) $
        fail "library loading should have failed with LUA_ERRRUN"

  , testCase "loading the re placeholder library causes an error" $ do
      l <- hsluaL_newstate
      lua_pushcclosure l luaopen_re_ptr 0
      stts <- lua_pcall l 0 1 0
      stts `seq` lua_close l
      when (stts /= LUA_ERRRUN) $
        fail "library loading should have failed with LUA_ERRRUN"

  , testCase "searcher returns message" $ do
      l <- hsluaL_newstate
      luaL_openlibs l
      _ <- withCString "package" $ lua_getglobal l
      pushstring l "searchers"
      _ <- lua_gettable l (-2)
      hslua_pushhsfunction l lpeg_searcher
      lua_rawseti l (-2) 1
      -- remove other searchers
      lua_pushnil l *> lua_rawseti l (-2) 2
      lua_pushnil l *> lua_rawseti l (-2) 3
      lua_pushnil l *> lua_rawseti l (-2) 4
      lua_pop l 2  -- tables 'package' and 'searchers'

      let ensureRequireFails lib = do
            stts <- withCStringLen ("require '" ++ lib ++ "'") $ \(s, slen) ->
              withCString "test script" $
                luaL_loadbuffer l s (fromIntegral slen)
            when (stts /= LUA_OK) $ do
              fail =<< peekCString =<< lua_tolstring l (-1) nullPtr
            callStatus <- lua_pcall l 0 0 0
            case callStatus of
              LUA_OK -> fail "require should have failed"
              LUA_ERRRUN -> return ()
              _ -> fail =<< peekCString =<< lua_tolstring l (-1) nullPtr

      ensureRequireFails "lpeg"
      ensureRequireFails "re"
      lua_close l
  ]
#endif

pushstring :: State -> String -> IO ()
pushstring l s = void $ withCString s $ lua_pushstring l
