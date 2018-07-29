{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Core.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Wrappers for the auxiliary library.
-}
module Foreign.Lua.Core.Auxiliary
  ( dostring
  , dofile
  , loadbuffer
  , loadfile
  , loadstring
  , newmetatable
  , newstate
  , tostring'
  , ref
  , unref
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Foreign.C (CChar, CInt (CInt), CSize (CSize), CString)
import Foreign.Lua.Core.Constants (multret, registryindex)
import Foreign.Lua.Core.Error (hsluaErrorRegistryField, throwTopMessageAsError)
import Foreign.Lua.Core.Types ( Lua, StackIndex, Status, liftLua, liftIO)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Foreign.Lua.Core.Functions as Lua
import qualified Foreign.Lua.Core.Types as Lua
import qualified Foreign.Lua.Utf8 as Utf8
import qualified Foreign.Storable as Storable

##ifdef ALLOW_UNSAFE_GC
##define SAFTY unsafe
##else
##define SAFTY safe
##endif


--------------------------------------------------------------------------------
-- * The Auxiliary Library

-- | Loads and runs the given string.
--
-- Returns @'OK'@ on success, or an error if either loading of the string or
-- calling of the thunk failed.
dostring :: ByteString -> Lua Status
dostring s = do
  loadRes <- loadstring s
  if loadRes == Lua.OK
    then Lua.pcall 0 multret Nothing
    else return loadRes

-- | Loads and runs the given file. Note that the filepath is interpreted by
-- Haskell, not Lua. The resulting chunk is named using the UTF8 encoded
-- filepath.
dofile :: FilePath -> Lua Status
dofile fp = do
  loadRes <- loadfile fp
  if loadRes == Lua.OK
    then Lua.pcall 0 multret Nothing
    else return loadRes

-- | Loads a ByteString as a Lua chunk.
--
-- This function returns the same results as @'load'@. @name@ is the chunk name,
-- used for debug information and error messages. Note that @name@ is used as a
-- C string, so it may not contain null-bytes.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadbuffer luaL_loadbuffer>.
loadbuffer :: ByteString -- ^ Program to load
           -> ByteString -- ^ chunk name
           -> Lua Status
loadbuffer bs name = liftLua $ \l ->
  B.useAsCStringLen bs $ \(str, len) ->
  B.useAsCString name $ \namePtr ->
  Lua.toStatus <$> luaL_loadbuffer l str (fromIntegral len) namePtr

foreign import capi SAFTY "lauxlib.h luaL_loadbuffer"
  luaL_loadbuffer :: Lua.State -> Ptr CChar -> CSize -> CString
                  -> IO Lua.StatusCode


-- | Loads a file as a Lua chunk. This function uses @lua_load@ (see @'load'@)
-- to load the chunk in the file named filename. The first line in the file is
-- ignored if it starts with a @#@.
--
-- The string mode works as in function @'load'@.
--
-- This function returns the same results as @'load'@, but it has an extra error
-- code @'ErrFile'@ for file-related errors (e.g., it cannot open or read the
-- file).
--
-- As @'load'@, this function only loads the chunk; it does not run it.
--
-- Note that the file is opened by Haskell, not Lua.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: FilePath -- ^ filename
         -> Lua Status
loadfile fp =
  liftIO (try (B.readFile fp) :: IO (Either IOException ByteString)) >>= \case
    Right script -> loadbuffer script (Utf8.fromString fp)
    Left e -> do
      Lua.pushstring (Utf8.fromString (show e))
      return Lua.ErrFile


-- | Loads a string as a Lua chunk. This function uses @lua_load@ to load the
-- chunk in the given ByteString. The given string may not contain any NUL
-- characters.
--
-- This function returns the same results as @lua_load@ (see @'load'@).
--
-- Also as @'load'@, this function only loads the chunk; it does not run it.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: ByteString -> Lua Status
loadstring str = loadbuffer str str


-- | If the registry already has the key tname, returns @False@. Otherwise,
-- creates a new table to be used as a metatable for userdata, adds to this new
-- table the pair @__name = tname@, adds to the registry the pair @[tname] = new
-- table@, and returns @True@. (The entry @__name@ is used by some
-- error-reporting functions.)
--
-- In both cases pushes onto the stack the final value associated with @tname@ in
-- the registry.
--
-- The value of @tname@ is used as a C string and hence must not contain null
-- bytes.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: ByteString -> Lua Bool
newmetatable tname = liftLua $ \l ->
  Lua.fromLuaBool <$> B.useAsCString tname (luaL_newmetatable l)

foreign import ccall SAFTY "lauxlib.h luaL_newmetatable"
  luaL_newmetatable :: Lua.State -> CString -> IO Lua.LuaBool


-- | Creates a new Lua state. It calls @'lua_newstate'@ with an allocator based
-- on the standard C @realloc@ function and then sets a panic function (see
-- <https://www.lua.org/manual/5.3/manual.html#4.6 §4.6> of the Lua 5.3
-- Reference Manual) that prints an error message to the standard error output
-- in case of fatal errors.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO Lua.State
newstate = do
  l <- luaL_newstate
  Lua.runWith l $ do
    Lua.createtable 0 0
    Lua.setfield registryindex hsluaErrorRegistryField
    return l

foreign import ccall unsafe "lauxlib.h luaL_newstate"
  luaL_newstate :: IO Lua.State


-- | Creates and returns a reference, in the table at index @t@, for the object
-- at the top of the stack (and pops the object).
--
-- A reference is a unique integer key. As long as you do not manually add
-- integer keys into table @t@, @ref@ ensures the uniqueness of the key it
-- returns. You can retrieve an object referred by reference @r@ by calling
-- @rawgeti t r@. Function @'unref'@ frees a reference and its associated
-- object.
--
-- If the object at the top of the stack is nil, @'ref'@ returns the constant
-- @'refnil'@. The constant @'noref'@ is guaranteed to be different from any
-- reference returned by @'ref'@.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#luaL_ref luaL_ref>.
ref :: StackIndex -> Lua Int
ref t = liftLua $ \l -> fromIntegral <$> luaL_ref l t

foreign import ccall SAFTY "lauxlib.h luaL_ref"
  luaL_ref :: Lua.State -> StackIndex -> IO CInt


-- | Converts any Lua value at the given index to a @'ByteString'@ in a
-- reasonable format. The resulting string is pushed onto the stack and also
-- returned by the function.
--
-- If the value has a metatable with a @__tostring@ field, then @tolstring'@
-- calls the corresponding metamethod with the value as argument, and uses the
-- result of the call as its result.
tostring' :: StackIndex -> Lua B.ByteString
tostring' n = liftLua $ \l -> alloca $ \lenPtr -> do
  cstr <- hsluaL_tolstring l n lenPtr
  if cstr == nullPtr
    then Lua.runWith l throwTopMessageAsError
    else do
      cstrLen <- Storable.peek lenPtr
      B.packCStringLen (cstr, fromIntegral cstrLen)

foreign import ccall safe "error-conversion.h hsluaL_tolstring"
  hsluaL_tolstring :: Lua.State -> StackIndex -> Ptr CSize -> IO (Ptr CChar)


-- | Releases reference @'ref'@ from the table at index @idx@ (see @'ref'@). The
-- entry is removed from the table, so that the referred object can be
-- collected. The reference @'ref'@ is also freed to be used again.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -> Int -> Lua ()
unref idx r = liftLua $ \l ->
  luaL_unref l idx (fromIntegral r)

foreign import ccall SAFTY "lauxlib.h luaL_unref"
  luaL_unref :: Lua.State -> StackIndex -> CInt -> IO ()
