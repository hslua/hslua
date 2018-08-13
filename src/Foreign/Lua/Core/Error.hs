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
  , catchException
  , throwException
  , modifyException
  , throwTopMessage
  , try
    -- * Helpers for hslua C wrapper functions.
  , Failable (..)
  , fromFailable
  , throwOnError
  , boolFromFailable
    -- * Signaling errors to Lua
  , hsluaErrorRegistryField
  ) where

import Control.Applicative (Alternative (..))
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Foreign.C (CChar, CInt (CInt), CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Lua.Core.Types (Lua, StackIndex, fromLuaBool)

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
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
throwException :: ByteString -> Lua a
throwException = Catch.throwM . Exception
{-# INLINABLE throwException #-}

-- | Catch a Lua @'Exception'@.
catchException :: Lua a -> (Exception -> Lua a) -> Lua a
catchException = Catch.catch
{-# INLINABLE catchException #-}

-- | Catch Lua @'Exception'@, alter the error message and rethrow.
modifyException :: Lua a -> (ByteString -> ByteString) -> Lua a
modifyException luaOp modifier =
  luaOp `catchException` \(Exception msg) -> throwException (modifier msg)
{-# INLINABLE modifyException #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
try :: Lua a -> Lua (Either Exception a)
try = Catch.try
{-# INLINABLE try #-}

instance Alternative Lua where
  empty = throwException "empty"
  x <|> y = either (const y) return =<< try x

-- | Convert the object at the top of the stack into a string and throw it as
-- an @'Exception'@.
throwTopMessage :: Lua a
throwTopMessage = do
  l <- Lua.state
  msg <- Lua.liftIO (errorMessage l)
  throwException msg

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

-- | Registry field under which the special HsLua error indicator is stored.
hsluaErrorRegistryField :: ByteString
hsluaErrorRegistryField = Char8.pack "HSLUA_ERR"

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
  then throwTopMessage
  else return (fromCInt x)

-- | Throw a Haskell exception if the computation signaled a failure.
throwOnError :: Failable () -> Lua ()
throwOnError = fromFailable (const ())

-- | Convert lua boolean to Haskell Bool, throwing an exception if the return
-- value indicates that an error had happened.
boolFromFailable :: Failable Lua.LuaBool -> Lua Bool
boolFromFailable = fmap fromLuaBool . fromFailable Lua.LuaBool
