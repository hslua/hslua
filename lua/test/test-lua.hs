{-|
Module      : Main
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Tests for the raw Lua bindings.
-}
module Main (main) where

import Foreign.C.String (peekCString, withCStringLen)
import Foreign.Ptr (nullPtr)
import Foreign.Lua (withNewState)
import Foreign.Lua.Auxiliary
import Foreign.Lua.Functions
import Foreign.Lua.Types
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ( Assertion, testCase, (@=?) )

-- | Runs tests.
main :: IO ()
main = defaultMain tests

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "lua"
  [ testGroup "state"
    [ "create and close" =: do
      l <- hsluaL_newstate
      lua_close l
    ]

  , testGroup "booleans"
    [ "push and retrieve" =: do
        b <- withNewState $ \l -> do
          lua_pushboolean l true
          lua_toboolean l (-1)
        true @=? b

    , "type" =: do
        ty <- withNewState $ \l -> do
          lua_pushboolean l false
          lua_type l (-1)
        TypeBoolean @=? toType ty
    ]

  , testGroup "numbers"
    [ "push and retrieve" =: do
        i <- withNewState $ \l -> do
          lua_pushinteger l 5
          lua_tointegerx l (-1) nullPtr
        5 @=? i

    , "type" =: do
        ty <- withNewState $ \l -> do
          lua_pushinteger l 0
          lua_type l (-1)
        TypeNumber @=? toType ty
    ]

  , testGroup "strings"
    [ "push and retrieve" =: do
        str <- withNewState $ \l -> do
          withCStringLen "testing" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          peekCString =<< lua_tolstring l (-1) nullPtr
        "testing" @=? str

    , "type" =: do
        ty <- withNewState $ \l -> do
          withCStringLen "Olsen Olsen" $ \(ptr, len) ->
            lua_pushlstring l ptr (fromIntegral len)
          lua_type l (-1)
        TypeString @=? toType ty
    ]

  , testGroup "constants"
    [ "loadedTableRegistryField"  =:
      ("_LOADED"  @=? loadedTableRegistryField)
    , "preloadTableRegistryField" =:
      ("_PRELOAD" @=? preloadTableRegistryField)
    ]
  ]

infix  3 =:
(=:) :: String -> Assertion -> TestTree
(=:) = testCase
