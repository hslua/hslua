{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor #-}
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
  , Failable (..)
  , fromFailable
  , throwOnError
  , throwMessage
  , boolFromFailable
    -- * Signaling errors to Lua
  , hsluaErrorRegistryField
  ) where

import Control.Applicative (Alternative (..))
import Data.Typeable (Typeable)
import Foreign.C (CChar, CInt (CInt), CSize (CSize))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Lua.Core.Types (Lua, StackIndex, fromLuaBool)

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.Storable as Storable
import qualified Foreign.Lua.Core.Types as Lua
import qualified Foreign.Lua.Utf8 as Utf8

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

-- | Helper function which uses proper error-handling to throw an
-- exception with the given message.
throwMessage :: String -> Lua a
throwMessage msg = do
  Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)
  Lua.errorConversion >>= Lua.liftLua . Lua.errorToException

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

foreign import capi unsafe "lua.h lua_pushlstring"
  lua_pushlstring :: Lua.State -> Ptr CChar -> CSize -> IO ()

-- | Registry field under which the special HsLua error indicator is stored.
hsluaErrorRegistryField :: String
hsluaErrorRegistryField = "HSLUA_ERR"

--
-- * Custom protocol to communicate with hslua C wrapper functions.
--

-- | CInt value or an error, using the convention that value below zero indicate
-- an error. Values greater than zero are used verbatim. The phantom type is
-- used for additional type safety and gives the type into which the wrapped
-- CInt should be converted.
newtype Failable a = Failable CInt

-- | Convert from Failable to target type if possible, returning
-- @'Nothing'@ if there was a failure.
fromFailable :: (CInt -> a) -> Failable a -> Lua (Result a)
fromFailable fromCInt (Failable x) =
  if x < 0
  then return ErrorOnStack
  else return (Success $ fromCInt x)

-- | The result of a Lua operation which may throw an error.
data Result a
  = Success a
  | ErrorOnStack
  deriving
    ( Eq
    , Functor
    , Ord
    , Show
    )

-- | Return a result or throw the error object at the top of the
-- stack as an exception.
forceResult :: Result a -> Lua a
forceResult = \case
  ErrorOnStack -> throwTopMessage
  Success x -> return x

-- | Throw a Haskell exception if the computation signaled a failure.
throwOnError :: Failable () -> Lua ()
throwOnError x = fromFailable (const ()) x >>= forceResult

-- | Convert lua boolean to Haskell Bool, throwing an exception if the return
-- value indicates that an error had happened.
boolFromFailable :: Failable Lua.LuaBool -> Lua Bool
boolFromFailable x = fromFailable Lua.LuaBool x
  >>= (fmap fromLuaBool . forceResult)
