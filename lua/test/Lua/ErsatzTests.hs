{-|
Module      : Lua.ErsatzTests
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Tests for Lua bindings.
-}
module Lua.ErsatzTests (tests) where

import Control.Monad (void)
import Foreign.C.String (withCStringLen)
import Foreign.Ptr (nullPtr)
import Lua
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, testCase, (@=?) )

-- | Tests for unsafe methods.
tests :: TestTree
tests = testGroup "ersatz"
  [ testGroup "global"
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
  ]

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase

shouldBeResultOf :: (HasCallStack, Eq a, Show a)
                 => a -> (State -> IO a) -> Assertion
shouldBeResultOf expected luaOp = do
  result <- withNewState luaOp
  expected @=? result
