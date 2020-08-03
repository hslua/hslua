{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-|
Module      : Foreign.Lua.Core.Error
Copyright   : © 2017-2020 Albert Krewinkel
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
  , withExceptionMessage
  , throwErrorAsException
  , throwTopMessage
  , throwTopMessageWithState
  , errorMessage
  , try
    -- * Helpers for hslua C wrapper functions.
  , throwMessage
  , liftLuaThrow
    -- * Signaling errors to Lua
  , hsluaErrorRegistryField
  ) where

import Control.Applicative (Alternative (..))
import Data.Typeable (Typeable)
import Foreign.Lua.Core.Types (Lua)
import Foreign.Lua.Raw.Error
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import qualified Foreign.Lua.Core.Types as Lua
import qualified Foreign.Lua.Utf8 as Utf8
import qualified Foreign.Storable as F

-- | Exceptions raised by Lua-related operations.
newtype Exception = Exception { exceptionMessage :: String}
  deriving (Eq, Typeable)

instance Show Exception where
  show (Exception msg) = "Lua exception: " ++ msg

instance E.Exception Exception

-- | Raise a Lua @'Exception'@ containing the given error message.
throwException :: String -> Lua a
throwException = Catch.throwM . Exception
{-# INLINABLE throwException #-}

-- | Catch a Lua @'Exception'@.
catchException :: Lua a -> (Exception -> Lua a) -> Lua a
catchException = Catch.catch
{-# INLINABLE catchException #-}

-- | Catch Lua @'Exception'@, alter the message and rethrow.
withExceptionMessage :: (String -> String) -> Lua a -> Lua a
withExceptionMessage modifier luaOp =
  luaOp `catchException` \(Exception msg) -> throwException (modifier msg)
{-# INLINABLE withExceptionMessage #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
try :: Lua a -> Lua (Either Exception a)
try = Catch.try
{-# INLINABLE try #-}

-- | Convert a Lua error into a Haskell exception. The error message is
-- expected to be at the top of the stack.
throwErrorAsException :: Lua a
throwErrorAsException = do
  f <- Lua.errorToException <$> Lua.errorConversion
  l <- Lua.state
  Lua.liftIO (f l)

-- | Alias for `throwErrorAsException`; will be deprecated in the next
-- mayor release.
throwTopMessage :: Lua a
throwTopMessage = throwErrorAsException

instance Alternative Lua where
  empty = throwMessage "empty"
  x <|> y = do
    alt <- Lua.alternative <$> Lua.errorConversion
    x `alt` y

-- | Convert the object at the top of the stack into a string and throw
-- it as a HsLua @'Exception'@.
--
-- This function serves as the default to convert Lua errors to Haskell
-- exceptions.
throwTopMessageWithState :: Lua.State -> IO a
throwTopMessageWithState l = do
  msg <- Lua.liftIO (errorMessage l)
  Catch.throwM $ Exception (Utf8.toString msg)

-- | Registry field under which the special HsLua error indicator is stored.
hsluaErrorRegistryField :: String
hsluaErrorRegistryField = "HSLUA_ERR"

-- | Takes a failable HsLua function and transforms it into a
-- monadic 'Lua' operation. Throws an exception if an error
-- occured.
liftLuaThrow :: (Lua.State -> Ptr Lua.StatusCode -> IO a) -> Lua a
liftLuaThrow f = do
  (result, status) <- Lua.liftLua $ \l -> alloca $ \statusPtr -> do
    result <- f l statusPtr
    status <- Lua.toStatus <$> F.peek statusPtr
    return (result, status)
  if status == Lua.OK
    then return result
    else throwTopMessage
