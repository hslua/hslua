{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| Instances for QuickCheck's Arbitrary. -}
module Lua.Arbitrary () where

import Lua
import Test.QuickCheck

instance Arbitrary Lua.Integer where
  arbitrary = arbitrarySizedIntegral
  shrink = shrinkIntegral

instance Arbitrary Lua.Number where
  arbitrary = Lua.Number <$> arbitrary
  shrink = shrinkRealFrac

instance Arbitrary Lua.TypeCode where
  arbitrary = elements
    [ LUA_TNONE
    , LUA_TNIL
    , LUA_TBOOLEAN
    , LUA_TLIGHTUSERDATA
    , LUA_TNUMBER
    , LUA_TSTRING
    , LUA_TTABLE
    , LUA_TFUNCTION
    , LUA_TUSERDATA
    , LUA_TTHREAD
    ]
  shrink = shrinkNothing

instance Arbitrary Lua.StatusCode where
  arbitrary = elements
    [ LUA_OK
    , LUA_YIELD
    , LUA_ERRRUN
    , LUA_ERRSYNTAX
    , LUA_ERRMEM
#if !MIN_VERSION_lua(2,2,0)
    , LUA_ERRGCMM
#endif
    , LUA_ERRERR
    , LUA_ERRFILE
    ]
  shrink = shrinkNothing

instance Arbitrary Lua.OPCode where
  arbitrary = elements
    [ LUA_OPEQ
    , LUA_OPLT
    , LUA_OPLE
    ]
  shrink = shrinkNothing
