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
import Foreign.Lua.Raw.Auxiliary
import Foreign.Lua.Raw.Functions
import Foreign.Lua.Raw.Types
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
      l <- hsluaL_newstate
      lua_pushboolean l true
      b <- lua_toboolean l (-1)
      lua_close l
      true @=? b

    , "type" =: do
        l <- hsluaL_newstate
        lua_pushboolean l false
        ty <- lua_type l (-1)
        lua_close l
        TypeBoolean @=? toType ty
    ]

  , testGroup "numbers"
    [ "push and retrieve" =: do
      l <- hsluaL_newstate
      lua_pushinteger l 5
      i <- lua_tointegerx l (-1) nullPtr
      lua_close l
      5 @=? i

    , "type" =: do
        l <- hsluaL_newstate
        lua_pushinteger l 0
        ty <- lua_type l (-1)
        lua_close l
        TypeNumber @=? toType ty
    ]

  , testGroup "strings"
    [ "push and retrieve" =: do
      l <- hsluaL_newstate
      withCStringLen "testing" $ \(ptr, len) ->
        lua_pushlstring l ptr (fromIntegral len)
      str <- peekCString =<< lua_tolstring l (-1) nullPtr
      lua_close l
      "testing" @=? str

    , "type" =: do
        l <- hsluaL_newstate
        withCStringLen "Olsen Olsen" $ \(ptr, len) ->
          lua_pushlstring l ptr (fromIntegral len)
        ty <- lua_type l (-1)
        lua_close l
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
