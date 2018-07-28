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
  ( Exception (..)
  , catchLuaError
  , throwLuaError
  , throwTopMessageAsError
  , modifyLuaError
  , tryLua
    -- * Helpers for hslua C wrapper functions.
  , Failable (..)
  , fromFailable
  , throwOnError
  , boolFromFailable
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Catch (catch, throwM, try)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Foreign.C (CChar, CInt (CInt), CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Lua.Core.Types (Lua, StackIndex, fromLuaBool)

import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.Storable as Storable
import qualified Foreign.Lua.Core.Types as Lua
import qualified Foreign.Lua.Utf8 as Utf8

-- | Exceptions raised by Lua-related operations.
newtype Exception = Exception ByteString
  deriving (Eq, Typeable)

instance Show Exception where
  show (Exception msg) = "Lua exception: " ++ Utf8.toString msg

instance E.Exception Exception

-- | Raise a Lua @'Exception'@ containing the given error message.
throwLuaError :: ByteString -> Lua a
throwLuaError = throwM . Exception
{-# INLINABLE throwLuaError #-}

-- | Catch a Lua @'Exception'@.
catchLuaError :: Lua a -> (Exception -> Lua a) -> Lua a
catchLuaError = catch
{-# INLINABLE catchLuaError #-}

-- | Catch Lua @'Exception'@, alter the error message and rethrow.
modifyLuaError :: Lua a -> (ByteString -> ByteString) -> Lua a
modifyLuaError luaOp modifier =
  luaOp `catchLuaError` \(Exception msg) -> throwLuaError (modifier msg)
{-# INLINABLE modifyLuaError #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
tryLua :: Lua a -> Lua (Either Exception a)
tryLua = try
{-# INLINABLE tryLua #-}

instance Alternative Lua where
  empty = throwLuaError "empty"
  x <|> y = either (const y) return =<< tryLua x

-- | Convert the object at the top of the stack into a string and throw it as
-- an @'Exception'@.
throwTopMessageAsError :: Lua a
throwTopMessageAsError = do
  l <- Lua.state
  msg <- Lua.liftIO (errorMessage l)
  throwLuaError msg

-- | Retrieve and pop the top object as an error message. This is very similar
-- to tostring', but ensures that we don't recurse if getting the message
-- failed.
errorMessage :: Lua.State -> IO B.ByteString
errorMessage l = alloca $ \lenPtr -> do
  cstr <- hsluaL_tolstring l Lua.stackTop lenPtr
  if cstr == nullPtr
    then return $ Char8.pack ("An error occurred, but the error object " ++
                              "cannot be converted into a string.")
    else do
      cstrLen <- Storable.peek lenPtr
      msg <- B.packCStringLen (cstr, fromIntegral cstrLen)
      lua_pop l 2
      return msg

foreign import ccall safe "error-conversion.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

foreign import capi unsafe "lua.h lua_pop"
  lua_pop :: Lua.State -> CInt -> IO ()

--
-- * Custom protocol to communicate with hslua C wrapper functions.
--

-- | CInt value or an error, using the convention that value below zero indicate
-- an error. Values greater than zero are used verbatim. The phantom type is
-- used for additional type safety and gives the type into which the wrapped
-- CInt should be converted.
newtype Failable a = Failable CInt

-- | Convert from Failable to target type, throwing an error if the value
-- indicates a failure.
fromFailable :: (CInt -> a) -> Failable a -> Lua a
fromFailable fromCInt (Failable x) =
  if x < 0
  then throwTopMessageAsError
  else return (fromCInt x)

-- | Throw a lua error if the computation signaled a failure.
throwOnError :: Failable () -> Lua ()
throwOnError = fromFailable (const ())

-- | Convert lua boolean to Haskell Bool, throwing an exception if the return
-- value indicates that an error had happened.
boolFromFailable :: Failable Lua.LuaBool -> Lua Bool
boolFromFailable = fmap fromLuaBool . fromFailable Lua.LuaBool
