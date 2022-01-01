{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
Module      : HsLua.Core.Unsafe
Copyright   : © 2019-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Unsafe Lua functions.

This module exports functions which conflict with those in 'HsLua.Core'.
It is intended to be imported qualified.
-}
module HsLua.Core.Unsafe
  ( next
  )
where

import Control.Monad ((<$!>))
import HsLua.Core.Types (LuaE, StackIndex, liftLua, fromLuaBool)
import Lua.Primary (lua_next)

-- | Wrapper for 'lua_next'.
--
-- __WARNING__: @lua_next@ is unsafe in Haskell: This function will
-- cause an unrecoverable crash an error if the given key is neither
-- @nil@ nor present in the table. Consider using the safe
-- @'HsLua.Core.next'@ function in HsLua.Core instead.
next :: StackIndex -> LuaE e Bool
next idx = liftLua $ \l -> fromLuaBool <$!> lua_next l idx
{-# INLINABLE next #-}
