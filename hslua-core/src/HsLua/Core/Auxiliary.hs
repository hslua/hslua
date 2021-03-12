{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.Core.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Wrappers for the auxiliary library.
-}
module HsLua.Core.Auxiliary
  ( dostring
  , dofile
  , getmetafield
  , getmetatable'
  , getsubtable
  , loadbuffer
  , loadfile
  , loadstring
  , newmetatable
  , newstate
  , tostring'
  , traceback
  -- * References
  , getref
  , ref
  , unref
  -- * Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
  ) where

import Control.Exception (IOException, try)
import Control.Monad ((<$!>))
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Foreign.C (withCString)
import HsLua.Core.Error (LuaError, throwErrorAsException)
import HsLua.Core.Types
  (LuaE, Name (Name), Status, StackIndex, liftLua, multret, runWith)
import Lua (top)
import Lua.Auxiliary
import Lua.Ersatz.Auxiliary
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified HsLua.Core.Functions as Lua
import qualified HsLua.Core.Types as Lua
import qualified HsLua.Core.Utf8 as Utf8
import qualified Foreign.Storable as Storable

-- * The Auxiliary Library

-- | Loads and runs the given string.
--
-- Returns 'Lua.OK' on success, or an error if either loading of the
-- string or calling of the thunk failed.
dostring :: ByteString -> LuaE e Status
dostring s = do
  loadRes <- loadstring s
  if loadRes == Lua.OK
    then Lua.pcall 0 multret Nothing
    else return loadRes

-- | Loads and runs the given file. Note that the filepath is interpreted by
-- Haskell, not Lua. The resulting chunk is named using the UTF8 encoded
-- filepath.
dofile :: FilePath -> LuaE e Status
dofile fp = do
  loadRes <- loadfile fp
  if loadRes == Lua.OK
    then Lua.pcall 0 multret Nothing
    else return loadRes

-- | Pushes onto the stack the field @e@ from the metatable of the object at
-- index @obj@ and returns the type of the pushed value. If the object does not
-- have a metatable, or if the metatable does not have this field, pushes
-- nothing and returns TypeNil.
getmetafield :: StackIndex -- ^ obj
             -> String     -- ^ e
             -> LuaE e Lua.Type
getmetafield obj e = liftLua $ \l ->
  withCString e $! fmap Lua.toType . luaL_getmetafield l obj

-- | Pushes onto the stack the metatable associated with name @tname@ in the
-- registry (see @newmetatable@) (@nil@ if there is no metatable associated
-- with that name). Returns the type of the pushed value.
getmetatable' :: String -- ^ tname
              -> LuaE e Lua.Type
getmetatable' tname = liftLua $ \l ->
  withCString tname $! fmap Lua.toType . luaL_getmetatable l

-- | Push referenced value from the table at the given index.
getref :: LuaError e => StackIndex -> Reference -> LuaE e ()
getref idx ref' = Lua.rawgeti idx (fromIntegral (Lua.fromReference ref'))

-- | Ensures that the value @t[fname]@, where @t@ is the value at index @idx@,
-- is a table, and pushes that table onto the stack. Returns True if it finds a
-- previous table there and False if it creates a new table.
getsubtable :: LuaError e
            => StackIndex   -- ^ idx
            -> Name         -- ^ fname
            -> LuaE e Bool
getsubtable idx fname@(Name namestr) = do
  -- This is a reimplementation of luaL_getsubtable from lauxlib.c.
  idx' <- Lua.absindex idx
  Lua.pushstring namestr
  Lua.gettable idx' >>= \case
    Lua.TypeTable -> return True
    _ -> do
      Lua.pop 1
      Lua.newtable
      Lua.pushvalue top -- copy to be left at top
      Lua.setfield idx' fname
      return False

-- | Loads a ByteString as a Lua chunk.
--
-- This function returns the same results as @'Lua.load'@. @name@ is the
-- chunk name, used for debug information and error messages. Note that
-- @name@ is used as a C string, so it may not contain null-bytes.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadbuffer luaL_loadbuffer>.
loadbuffer :: ByteString -- ^ Program to load
           -> Name       -- ^ chunk name
           -> LuaE e Status
loadbuffer bs (Name name) = liftLua $ \l ->
  B.useAsCStringLen bs $ \(str, len) ->
  B.useAsCString name $!
    fmap Lua.toStatus . luaL_loadbuffer l str (fromIntegral len)

-- | Loads a file as a Lua chunk. This function uses @lua_load@ (see
-- @'Lua.load'@) to load the chunk in the file named filename. The first
-- line in the file is ignored if it starts with a @#@.
--
-- The string mode works as in function @'Lua.load'@.
--
-- This function returns the same results as @'Lua.load'@, but it has an
-- extra error code @'Lua.ErrFile'@ for file-related errors (e.g., it
-- cannot open or read the file).
--
-- As @'Lua.load'@, this function only loads the chunk; it does not run
-- it.
--
-- Note that the file is opened by Haskell, not Lua.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: FilePath -- ^ filename
         -> LuaE e Status
loadfile fp = Lua.liftIO contentOrError >>= \case
  Right script -> loadbuffer script (fromString $ '@' : fp)
  Left e -> do
    Lua.pushstring (Utf8.fromString (show e))
    return Lua.ErrFile
 where
  contentOrError :: IO (Either IOException ByteString)
  contentOrError = try (B.readFile fp)


-- | Loads a string as a Lua chunk. This function uses @lua_load@ to
-- load the chunk in the given ByteString. The given string may not
-- contain any NUL characters.
--
-- This function returns the same results as @lua_load@ (see
-- @'Lua.load'@).
--
-- Also as @'Lua.load'@, this function only loads the chunk; it does not
-- run it.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: ByteString -> LuaE e Status
loadstring s = loadbuffer s (Name s)


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
newmetatable :: String -> LuaE e Bool
newmetatable tname = liftLua $ \l ->
  Lua.fromLuaBool <$!> withCString tname (luaL_newmetatable l)

-- | Creates a new Lua state. It calls @lua_newstate@ with an allocator
-- based on the standard C @realloc@ function and then sets a panic
-- function (see <https://www.lua.org/manual/5.3/manual.html#4.6 §4.6>
-- of the Lua 5.3 Reference Manual) that prints an error message to the
-- standard error output in case of fatal errors.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO Lua.State
newstate = hsluaL_newstate

-- | Creates and returns a reference, in the table at index @t@, for the object
-- at the top of the stack (and pops the object).
--
-- A reference is a unique integer key. As long as you do not manually add
-- integer keys into table @t@, @ref@ ensures the uniqueness of the key it
-- returns. You can retrieve an object referred by reference @r@ by calling
-- @rawgeti t r@. Function @'unref'@ frees a reference and its associated
-- object.
--
-- If the object at the top of the stack is nil, @'ref'@ returns the
-- constant @'Lua.refnil'@. The constant @'Lua.noref'@ is guaranteed to
-- be different from any reference returned by @'ref'@.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#luaL_ref luaL_ref>.
ref :: StackIndex -> LuaE e Reference
ref t = liftLua $ \l -> Lua.toReference <$> luaL_ref l t

-- | Converts any Lua value at the given index to a 'ByteString' in a
-- reasonable format. The resulting string is pushed onto the stack and
-- also returned by the function.
--
-- If the value has a metatable with a @__tostring@ field, then
-- @tolstring'@ calls the corresponding metamethod with the value as
-- argument, and uses the result of the call as its result.
tostring' :: forall e. LuaError e => StackIndex -> LuaE e B.ByteString
tostring' n = do
  l <- Lua.state
  Lua.liftIO $ alloca $ \lenPtr -> do
    cstr <- hsluaL_tolstring l n lenPtr
    if cstr == nullPtr
      then runWith @e l throwErrorAsException
      else do
        cstrLen <- Storable.peek lenPtr
        B.packCStringLen (cstr, fromIntegral cstrLen)

-- | Creates and pushes a traceback of the stack L1. If a message is given it
-- appended at the beginning of the traceback. The level parameter tells at
-- which level to start the traceback.
traceback :: Lua.State -> Maybe String -> Int -> LuaE e ()
traceback l1 msg level = liftLua $ \l ->
  case msg of
    Nothing -> luaL_traceback l l1 nullPtr (fromIntegral level)
    Just msg' -> withCString msg' $ \cstr ->
      luaL_traceback l l1 cstr (fromIntegral level)

-- | Releases reference @'ref'@ from the table at index @idx@ (see @'ref'@). The
-- entry is removed from the table, so that the referred object can be
-- collected. The reference @'ref'@ is also freed to be used again.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -- ^ idx
      -> Reference  -- ^ ref
      -> LuaE e ()
unref idx r = liftLua $ \l ->
  luaL_unref l idx (Lua.fromReference r)
