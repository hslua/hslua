{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Don't warn about lua_concat; the way it's use here is safe.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
Module      : HsLua.Core.Error
Copyright   : © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Lua exceptions and exception handling.
-}
module HsLua.Core.Error
  ( Exception (..)
  , LuaError (..)
  , Lua
  , try
  , failLua
  , throwErrorAsException
  , throwTypeMismatchError
  , changeErrorType
    -- * Helpers for hslua C wrapper functions.
  , liftLuaThrow
  , popErrorMessage
  , pushTypeMismatchError
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<$!>), void)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (Proxy))
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

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail (..))
#endif

-- | A Lua operation.
--
-- This type is suitable for most users. It uses a default exception for
-- error handling. Users who need more control over error handling can
-- use 'LuaE' with a custom error type instead.
type Lua a = LuaE Exception a

-- | Any type that you wish to use for error handling in HsLua must be
-- an instance of the @LuaError@ class.
class E.Exception e => LuaError e where
  -- | Converts the error at the top of the stack into an exception and
  -- pops the error off the stack.
  --
  -- This function is expected to produce a valid result for any Lua
  -- value; neither a Haskell exception nor a Lua error may result when
  -- this is called.
  popException :: LuaE e e
  -- | Pushes an exception to the top of the Lua stack. The pushed Lua
  -- object is used as an error object, and it is recommended that
  -- calling @tostring()@ on the object produces an informative message.
  pushException :: e -> LuaE e ()
  -- | Creates a new exception with the given message.
  luaException :: String -> e

-- | Default Lua error type. Exceptions raised by Lua-related operations.
newtype Exception = Exception { exceptionMessage :: String}
  deriving (Eq, Typeable)

instance Show Exception where
  show (Exception msg) = "Lua exception: " ++ msg

instance E.Exception Exception

instance LuaError Exception where
  popException = do
    Exception . Utf8.toString <$!> liftLua popErrorMessage
  {-# INLINABLE popException #-}

  pushException (Exception msg) = Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)
  {-# INLINABLE pushException #-}

  luaException = Exception
  {-# INLINABLE luaException #-}

-- | Return either the result of a Lua computation or, if an exception was
-- thrown, the error.
try :: Catch.Exception e => LuaE e a -> LuaE e (Either e a)
try = Catch.try
{-# INLINABLE try #-}

-- | Raises an exception in the Lua monad.
failLua :: forall e a. LuaError e => String -> LuaE e a
failLua msg = Catch.throwM (luaException @e msg)
{-# INLINABLE failLua #-}

-- | Converts a Lua error at the top of the stack into a Haskell
-- exception and throws it.
throwErrorAsException :: LuaError e => LuaE e a
throwErrorAsException = do
  err <- popException
  Catch.throwM $! err
{-# INLINABLE throwErrorAsException #-}

-- | Raises an exception that's appropriate when the type of a Lua
-- object at the given index did not match the expected type. The name
-- or description of the expected type is taken as an argument.
throwTypeMismatchError :: forall e a. LuaError e
                       => ByteString -> StackIndex -> LuaE e a
throwTypeMismatchError expected idx = do
  pushTypeMismatchError expected idx
  throwErrorAsException
{-# INLINABLE throwTypeMismatchError #-}

-- | Change the error type of a computation.
changeErrorType :: forall old new a. LuaE old a -> LuaE new a
changeErrorType op = Lua.liftLua $ \l -> do
  x <- Lua.runWith l op
  return $! x
{-# INLINABLE changeErrorType #-}


--
-- Orphan instances
--

instance LuaError e => Alternative (LuaE e) where
  empty = failLua "empty"
  x <|> y = x `Catch.catch` (\(_ :: e) -> y)

instance LuaError e => MonadFail (LuaE e) where
  fail = failLua

--
-- Helpers
--

-- | Takes a failable HsLua function and transforms it into a
-- monadic 'LuaE' operation. Throws an exception if an error
-- occured.
liftLuaThrow :: forall e a. LuaError e
             => (Lua.State -> Ptr Lua.StatusCode -> IO a)
             -> LuaE e a
liftLuaThrow f = Lua.liftLua (throwOnError (Proxy @e) f)

-- | Helper function which takes an ersatz function and checks for
-- errors during its execution. If an error occured, it is converted
-- into a 'LuaError' and thrown as an exception.
throwOnError :: forall e a. LuaError e
             => Proxy e
             -> (Lua.State -> Ptr Lua.StatusCode -> IO a)
             -> Lua.State
             -> IO a
throwOnError _errProxy f l = alloca $ \statusPtr -> do
  result <- f l statusPtr
  status <- Storable.peek statusPtr
  if status == LUA_OK
    then return $! result
    else Lua.runWith l (throwErrorAsException @e)


-- | Retrieve and pop the top object as an error message. This is very
-- similar to tostring', but ensures that we don't recurse if getting
-- the message failed.
--
-- This helpful as a \"last resort\" method when implementing
-- 'popException'.
popErrorMessage :: Lua.State -> IO ByteString
popErrorMessage l = alloca $ \lenPtr -> do
  cstr <- hsluaL_tolstring l (-1) lenPtr
  if cstr == nullPtr
    then do
      lua_pop l 1
      return $ Char8.pack
        "An error occurred, but the error object could not be retrieved."
    else do
      cstrLen <- Storable.peek lenPtr
      msg <- B.packCStringLen (cstr, fromIntegral cstrLen)
      lua_pop l 2  -- pop original msg and product of hsluaL_tolstring
      return msg

-- | Creates an error to notify about a Lua type mismatch and pushes it
-- to the stack.
pushTypeMismatchError :: ByteString  -- ^ name or description of expected type
                      -> StackIndex  -- ^ stack index of mismatching object
                      -> LuaE e ()
pushTypeMismatchError expected idx = liftLua $ \l -> do
  let pushtype = lua_type l idx >>= lua_typename l >>= lua_pushstring l
  B.unsafeUseAsCString "__name" (luaL_getmetafield l idx) >>= \case
    LUA_TSTRING -> return () -- pushed the name
    LUA_TNIL    -> void pushtype
    _           -> lua_pop l 1 <* pushtype
  let pushstring str = B.unsafeUseAsCStringLen str $ \(cstr, cstrLen) ->
        lua_pushlstring l cstr (fromIntegral cstrLen)
  pushstring expected
  pushstring " expected, got "
  lua_rotate l (-3) (-1)  -- move actual type to the end
  lua_concat l 3
