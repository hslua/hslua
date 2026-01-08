{-# LANGUAGE LambdaCase             #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-|
Module      : Lua.PrimaryTests
Copyright   : © 2021-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests for bindings to primary API functions.
-}
module Lua.PrimaryTests (tests) where

import Foreign.C (CInt (..), CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import Lua
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, assertBool, testCase, (@=?) )

-- | Tests for unsafe methods.
tests :: TestTree
tests = testGroup "Primary"
  [ testGroup "C functions"
    [ "can push and call luaopen_math" =: do
        LUA_TTABLE `shouldBeResultOf` \l -> do
          lua_pushcfunction l luaopen_math
          lua_pcall l 0 1 0
          lua_type l (-1)
    ]

  , testGroup "garbage-collection"
    [ "stop, restart GC"  =:
        -- first count should be larger
        uncurry (>) `shouldHoldForResultOf` \l -> do
          lua_createtable l 0 0
          _  <- lua_gc l LUA_GCSTOP 0 0 0
          lua_pop l 1
          kb1 <- lua_gc l LUA_GCCOUNT 0 0 0
          b1  <- lua_gc l LUA_GCCOUNTB 0 0 0
          _   <- lua_gc l LUA_GCCOLLECT 0 0 0
          kb2 <- lua_gc l LUA_GCCOUNT 0 0 0
          b2  <- lua_gc l LUA_GCCOUNTB 0 0 0
          return (b1 + 1024 * kb1, b2 + 1024 * kb2)
    , "switch to generational GC"  =:
        LUA_GCINC `shouldBeResultOf` \l -> do
          lua_createtable l 0 0
          GCCode <$> lua_gc l LUA_GCGEN 0 0 0
    , "switch to generational and back to incremental GC"  =:
        LUA_GCGEN `shouldBeResultOf` \l -> do
          lua_createtable l 0 0
          _ <- lua_gc l LUA_GCGEN 0 0 0
          GCCode <$> lua_gc l LUA_GCINC 0 0 0
    , "memory consumption should be between 0 and 10 kB" =:
      (\count -> count > 0 && count < 10) `shouldHoldForResultOf` \l -> do
          lua_gc l LUA_GCCOUNT 0 0 0
    ]

  , testGroup "constants"
    [ "LUA_RIDX_GLOBALS" =:
      TRUE `shouldBeResultOf` \l -> do
        lua_pushvalue l LUA_REGISTRYINDEX
        lua_rawgeti l (-1) LUA_RIDX_GLOBALS
        lua_pushglobaltable l
        lua_rawequal l (-1) (-2)
    ]

  , testGroup "lua_stringtonumber"
    [ "converts a string to a number" =: do
        55 `shouldBeResultOf` \l -> do
          _ <- withCString "55" $ lua_stringtonumber l
          lua_tointegerx l top nullPtr
    , "returns length (incl NULL) of the string on success" =: do
        4 `shouldBeResultOf` \l -> do
          withCString "512" $ lua_stringtonumber l
    , "returns zero on failure" =: do
        0 `shouldBeResultOf` \l -> do
          withCString "NaN" $ lua_stringtonumber l
    ]

  , testGroup "warnings"
    [ "collect warnings" =:
        "my warning" `shouldBeResultOf` \l -> do
          warnf <- makeWarnFunction warn
          let State ud = l
          lua_setwarnf l warnf ud
          withCString "my warning" $ \w -> lua_warning l w FALSE
          withCString "previous-warning" $ lua_pushstring l
          lua_rawget l LUA_REGISTRYINDEX
          lua_type l top >>= \case
            LUA_TSTRING -> peekCString =<< lua_tolstring l top nullPtr
            _ -> pure ""
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

shouldHoldForResultOf :: HasCallStack
                      => (a -> Bool) -> (State -> IO a) -> Assertion
shouldHoldForResultOf predicate luaOp = do
  result <- withNewState luaOp
  assertBool "predicate does not hold" (predicate result)

warn :: Ptr () -> CString -> LuaBool -> IO ()
warn udPtr msg _cont = do
  let l = State udPtr
  withCString "previous-warning" $ lua_pushstring l
  lua_pushstring l msg
  lua_rawset l LUA_REGISTRYINDEX

foreign import ccall "wrapper"
  makeWarnFunction :: (Ptr () -> CString -> LuaBool -> IO ())
                   -> IO WarnFunction
