{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-|
Module      : HsLua.Core.Error
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : DeriveDataTypeable

Lua exceptions and exception handling.
-}
module HsLua.Core.Error
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
  ) where

import Control.Applicative (Alternative (..))
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import HsLua.Core.Types (Lua)
import Lua (lua_pop, lua_pushlstring, hsluaL_tolstring)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.Storable as Storable
import qualified Lua.Types as Lua
import qualified Data.ByteString.Unsafe as B
import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import qualified HsLua.Core.Types as Lua
import qualified HsLua.Core.Utf8 as Utf8
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
  e <- Lua.errorConversion
  l <- Lua.state
  Lua.liftIO (Lua.errorToException e l)

-- | Alias for `throwErrorAsException`; will be deprecated in the next
-- mayor release.
throwTopMessage :: Lua a
throwTopMessage = throwErrorAsException

-- | Helper function which uses proper error-handling to throw an
-- exception with the given message.
throwMessage :: String -> Lua a
throwMessage msg = do
  Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)
  e <- Lua.errorConversion
  Lua.liftLua (Lua.errorToException e)

instance Alternative Lua where
  empty = throwMessage "empty"
  x <|> y = do
    e <- Lua.errorConversion
    Lua.alternative e x y

-- | Convert the object at the top of the stack into a string and throw
-- it as a HsLua @'Exception'@.
--
-- This function serves as the default to convert Lua errors to Haskell
-- exceptions.
throwTopMessageWithState :: Lua.State -> IO a
throwTopMessageWithState l = do
  msg <- Lua.liftIO (errorMessage l)
  Catch.throwM $ Exception (Utf8.toString msg)

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

-- | Retrieve and pop the top object as an error message. This is very similar
-- to tostring', but ensures that we don't recurse if getting the message
-- failed.
errorMessage :: Lua.State -> IO ByteString
errorMessage l = alloca $ \lenPtr -> do
  cstr <- hsluaL_tolstring l (-1) lenPtr
  if cstr == nullPtr
    then return $ Char8.pack ("An error occurred, but the error object " ++
                              "cannot be converted into a string.")
    else do
      cstrLen <- Storable.peek lenPtr
      msg <- B.packCStringLen (cstr, fromIntegral cstrLen)
      lua_pop l 2
      return msg
