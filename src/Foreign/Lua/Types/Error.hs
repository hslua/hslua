{-
Copyright © 2017 Albert Krewinkel

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
{-|
Module      : Foreign.Lua.Types.Error
Copyright   : © 2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : DeriveDataTypeable

Lua exceptions and exception handling.
-}
module Foreign.Lua.Types.Error
  ( LuaException (..)
  , catchLuaError
  , throwLuaError
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM, catch)
import Data.Typeable (Typeable)
import Foreign.Lua.Types.Lua (Lua)

data LuaException = LuaException String
  deriving (Typeable)

instance Show LuaException where
  show (LuaException err) = err

instance Exception LuaException

throwLuaError :: String -> Lua a
throwLuaError = throwM . LuaException

catchLuaError :: Lua a -> (LuaException -> Lua a) -> Lua a
catchLuaError = catch
