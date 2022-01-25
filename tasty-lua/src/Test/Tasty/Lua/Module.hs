{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Test.Tasty.Lua.Module
Copyright   : © 2019–2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Tasty Lua module, providing the functions necessary to write tasty tests
in Lua scripts.
-}
module Test.Tasty.Lua.Module
  ( pushModule )
where

import Data.ByteString (ByteString)
import Data.FileEmbed
import HsLua.Core
  ( HaskellFunction, LuaError, Status (OK)
  , dostringTrace, throwErrorAsException )

-- | Tasty Lua script
tastyScript :: ByteString
tastyScript = $(embedFile "tasty.lua")

-- | Push the tasty module on the Lua stack.
pushModule :: LuaError e => HaskellFunction e
pushModule = dostringTrace tastyScript >>= \case
  OK -> pure 1
  _  -> throwErrorAsException
{-# INLINABLE pushModule #-}
