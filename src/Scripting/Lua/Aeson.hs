{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      :  Scripting.Lua.Aeson
Copyright   :  © 2017 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>
Stability   :  experimental
Portability :  portable

Glue to hslua for aeson values.
-}
module Scripting.Lua.Aeson
  ( module Scripting.Lua
  ) where

import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Scripting.Lua (LuaState, StackValue)

import qualified Scripting.Lua as Lua

instance StackValue Scientific where
  push lua n = Lua.pushnumber lua (toRealFloat n)
  peek lua n = fmap fromFloatDigits <$>
               (Lua.peek lua n :: IO (Maybe Lua.LuaNumber))
  valuetype _ = Lua.TNUMBER

instance StackValue Text where
  push lua t = Lua.push lua (encodeUtf8 t)
  peek lua i = fmap decodeUtf8 <$> Lua.peek lua i
  valuetype _ = Lua.TSTRING
