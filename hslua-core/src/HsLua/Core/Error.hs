{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , LuaError (..)
  , Lua
  , catchException
  , throwException
  , withExceptionMessage
  , throwErrorAsException
  , throwTopMessage
  , throwTopMessageWithState
  , throwTypeMismatchError
  , errorMessage
  , try
    -- * Helpers for hslua C wrapper functions.
  , throwMessage
  , liftLuaThrow
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<$!>))
import Data.ByteString (ByteString, append)
import Data.Typeable (Typeable)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import HsLua.Core.Types (LuaE, liftLua)
import Lua

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.Storable as Storable
import qualified HsLua.Core.Types as Lua
import qualified HsLua.Core.Utf8 as Utf8

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

-- | Exceptions raised by Lua-related operations.
newtype Exception = Exception { exceptionMessage :: String}
  deriving (Eq, Typeable)

instance Show Exception where
  show (Exception msg) = "Lua exception: " ++ msg

instance E.Exception Exception

-- | A Lua operation.
--
-- This type is suitable for most users. It uses a default exception for
-- error handling. Users who need more control over error handling can
-- use 'LuaE' with a custom error type instead.
type Lua a = LuaE Exception a

-- | Any type that you wish to use for error handling in HsLua must be
-- an instance of the @LuaError@ class.
class E.Exception e => LuaError e where
  -- | Converts the error at the top of the stack into an exception.
  -- This function is expected to produce a valid result for any Lua
  -- value.
  peekException :: LuaE e e
  -- | Pushes an exception to the top of the Lua stack. The pushed Lua
  -- object is used as an error object, and it is recommended that
  -- calling @tostring()@ on the object produces an informative message.
  pushException :: e -> LuaE e ()

instance LuaError Exception where
  peekException = do
    Exception . Utf8.toString <$!> liftLua errorMessage
  pushException (Exception msg) = Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)

instance Semigroup Exception where
  Exception a <> Exception b = Exception (a ++ '\n' : b)

-- | Raise a Lua @'Exception'@ containing the given error message.
throwException :: String -> LuaE e a
throwException = Catch.throwM . Exception
{-# INLINABLE throwException #-}

-- | Catch a Lua @'Exception'@.
catchException :: LuaE e a -> (Exception -> LuaE e a) -> LuaE e a
catchException = Catch.catch
{-# INLINABLE catchException #-}

-- | Catch Lua @'Exception'@, alter the message and rethrow.
withExceptionMessage :: (String -> String) -> LuaE e a -> LuaE e a
withExceptionMessage modifier luaOp =
  luaOp `catchException` \(Exception msg) -> throwException (modifier msg)
{-# INLINABLE withExceptionMessage #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
try :: Catch.Exception e => LuaE e a -> LuaE e (Either e a)
try = Catch.try
{-# INLINABLE try #-}

-- | Convert a Lua error into a Haskell exception. The error message is
-- expected to be at the top of the stack.
throwErrorAsException :: LuaError e => LuaE e a
throwErrorAsException = do
  err <- peekException
  Catch.throwM err

-- | Alias for `throwErrorAsException`; will be deprecated in the next
-- mayor release.
throwTopMessage :: LuaError e => LuaE e a
throwTopMessage = throwErrorAsException

-- | Helper function which uses proper error-handling to throw an
-- exception with the given message.
throwMessage :: LuaError e => String -> LuaE e a
throwMessage msg = do
  Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)
  e <- peekException
  Catch.throwM e

instance LuaError e => Alternative (LuaE e) where
  empty = throwMessage "empty"
  x <|> y = x `Catch.catch` (\(_ :: e) -> y)

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
liftLuaThrow :: LuaError e
             => (Lua.State -> Ptr Lua.StatusCode -> IO a)
             -> LuaE e a
liftLuaThrow f = do
  (result, status) <- Lua.liftLua $ \l -> alloca $ \statusPtr -> do
    result <- f l statusPtr
    status <- Lua.toStatus <$> Storable.peek statusPtr
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

-- | Raises an exception that's appropriate when the type of a Lua
-- object at the given index did not match the expected type. The name
-- or description of the expected type is taken as an argument.
throwTypeMismatchError :: LuaError e
                       => ByteString  -- ^ name or description of expected type
                       -> StackIndex  -- ^ stack index of mismatching object
                       -> LuaE e a
throwTypeMismatchError expected idx = do
  Lua.liftLua $ \l -> do
    idx' <- lua_absindex l idx
    let pushstring str = B.unsafeUseAsCStringLen str $ \(cstr, cstrLen) ->
          lua_pushlstring l cstr (fromIntegral cstrLen)
    pushstring $ "expected " `append` expected `append` ", got '"
    _ <- hsluaL_tolstring l idx' nullPtr
    pushstring "' ("
    _ <- lua_type l idx' >>= lua_typename l >>= lua_pushstring l
    pushstring  ")"
    alloca $ hslua_concat l 5
  throwTopMessage
