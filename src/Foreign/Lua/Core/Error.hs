{-
Copyright © 2017-2018 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-|
Module      : Foreign.Lua.Core.Error
Copyright   : © 2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : DeriveDataTypeable

Lua exceptions and exception handling.
-}
module Foreign.Lua.Core.Error
  ( LuaException (..)
  , catchLuaError
  , throwLuaError
  , modifyLuaError
  , tryLua
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Catch (catch, throwM, try)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Foreign.Lua.Core.Types (Lua)

import qualified Data.ByteString.UTF8 as UTF8 (toString)

-- | Exceptions raised by Lua-related operations.
newtype LuaException = LuaException ByteString
  deriving (Eq, Typeable)

instance Show LuaException where
  show (LuaException msg) = "Lua exception: " ++ UTF8.toString msg

instance Exception LuaException

-- | Raise a @'LuaException'@ containing the given error message.
throwLuaError :: ByteString -> Lua a
throwLuaError = throwM . LuaException
{-# INLINABLE throwLuaError #-}

-- | Catch a @'LuaException'@.
catchLuaError :: Lua a -> (LuaException -> Lua a) -> Lua a
catchLuaError = catch
{-# INLINABLE catchLuaError #-}

-- | Catch @'LuaException'@, alter the error message and rethrow.
modifyLuaError :: Lua a -> (ByteString -> ByteString) -> Lua a
modifyLuaError luaOp modifier =
  luaOp `catchLuaError` \(LuaException msg) -> throwLuaError (modifier msg)
{-# INLINABLE modifyLuaError #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
tryLua :: Lua a -> Lua (Either LuaException a)
tryLua = try
{-# INLINABLE tryLua #-}

instance Alternative Lua where
  empty = throwLuaError "empty"
  x <|> y = either (const y) return =<< tryLua x
