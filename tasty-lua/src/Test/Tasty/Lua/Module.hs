{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Test.Tasty.Lua.Module
Copyright   : © 2019-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tasty Lua module, providing the functions necessary to write tasty tests
in Lua scripts.
-}
module Test.Tasty.Lua.Module
  ( pushModule )
where

import Data.ByteString (ByteString)
import Data.FileEmbed
import HsLua.Core
  ( HaskellFunction, LuaError, NumResults (..), Status (OK)
  , dostringTrace, nth, rawset, throwErrorAsException )
import HsLua.Marshalling (pushName)
import Test.Tasty.Lua.Arbitrary

-- | Tasty Lua script
tastyScript :: ByteString
tastyScript = $(embedFile "tasty.lua")

-- | Push the tasty module on the Lua stack.
pushModule :: LuaError e => HaskellFunction e
pushModule = dostringTrace tastyScript >>= \case
  OK -> NumResults 1 <$ do
    -- add `arbitrary` table
    pushName "arbitrary"
    pushArbitraryTable
    rawset (nth 3)
    registerDefaultGenerators
  _  -> throwErrorAsException
{-# INLINABLE pushModule #-}
