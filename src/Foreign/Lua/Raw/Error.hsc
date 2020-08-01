{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Raw.Error
Copyright   : © 2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : DeriveDataTypeable

Lua exceptions and exception handling.
-}
module Foreign.Lua.Raw.Error
  ( -- * Passing Lua errors to Haskell
    errorMessage
  , throwMessage
    -- * Helpers for HsLua C wrapper functions.
  , Failable (..)
  , fromFailable
    -- * Result
  , Result (..)
  ) where

import Data.ByteString (ByteString)
import Foreign.C (CChar, CInt (CInt), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Lua.Raw.Types (Lua, StackIndex)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.Storable as Storable
import qualified Foreign.Lua.Raw.Types as Lua
import qualified Foreign.Lua.Utf8 as Utf8

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

foreign import ccall safe "error-conversion.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)

foreign import capi unsafe "lua.h lua_pop"
  lua_pop :: Lua.State -> CInt -> IO ()

--
-- * Custom HsLua C wrapper error protocol.
--

-- | CInt value or an error, using the convention that values
-- below zero indicate an error. Values greater than zero are
-- used verbatim. The phantom type is used for additional type
-- safety and gives the type into which the wrapped CInt should
-- be converted.
newtype Failable a = Failable CInt

-- | Convert from Failable to target type if possible, returning
-- @'Nothing'@ if there was a failure.
fromFailable :: (CInt -> a) -> Failable a -> Lua (Result a)
fromFailable fromCInt (Failable x) =
  if x < 0
  then return ErrorOnStack
  else return (Success $ fromCInt x)

-- | Helper function which uses proper error-handling to throw an
-- exception with the given message.
throwMessage :: String -> Lua a
throwMessage msg = do
  Lua.liftLua $ \l ->
    B.unsafeUseAsCStringLen (Utf8.fromString msg) $ \(msgPtr, z) ->
      lua_pushlstring l msgPtr (fromIntegral z)
  Lua.errorConversion >>= Lua.liftLua . Lua.errorToException

foreign import capi unsafe "lua.h lua_pushlstring"
  lua_pushlstring :: Lua.State -> Ptr CChar -> CSize -> IO ()

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
