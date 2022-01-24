{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.Core.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Wrappers for the auxiliary library.
-}
module HsLua.Core.Auxiliary
  ( -- * The Auxiliary Library
    checkstack'
  , dostring
  , dofile
  , getmetafield
  , getmetatable'
  , getsubtable
  , loadbuffer
  , loadfile
  , loadstring
  , newmetatable
  , newstate
  , requiref
  , tostring'
  , traceback
  , where'
    -- ** References
  , getref
  , ref
  , unref
    -- ** Registry fields
  , loaded
  , preload
  ) where

import Control.Monad ((<$!>))
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import HsLua.Core.Error
import HsLua.Core.Types
  (LuaE, Name (Name), Status, StackIndex, liftLua, multret, runWith)
import Lua (top)
import Lua.Auxiliary
import Lua.Ersatz.Auxiliary
import Foreign.C (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified HsLua.Core.Primary as Lua
import qualified HsLua.Core.Types as Lua
import qualified Foreign.Storable as Storable

-- | Grows the stack size to @top + sz@ elements, raising an error if
-- the stack cannot grow to that size. @msg@ is an additional text to go
-- into the error message (or the empty string for no additional text).
checkstack' :: LuaError e
            => Int    -- ^ sz (requested additional size)
            -> String -- ^ msg
            -> LuaE e ()
checkstack' sz msg =
  Lua.checkstack sz >>= \case
    True  -> pure ()
    False -> failLua $
      if msg == ""
      then "stack overflow"
      else "stack overflow (" ++ msg ++ ")"

-- | Loads and runs the given string.
--
-- Returns 'Lua.OK' on success, or an error if either loading of the
-- string or calling of the thunk failed.
dostring :: ByteString -> LuaE e Status
dostring s = loadstring s >>= \case
  Lua.OK -> Lua.pcall 0 multret Nothing
  err    -> return err
{-# INLINABLE dostring #-}

-- | Loads and runs the given file. Note that the filepath is
-- interpreted by Lua, not Haskell. The resulting chunk is named using
-- the UTF8 encoded filepath.
dofile :: FilePath -> LuaE e Status
dofile fp = loadfile fp >>= \case
  Lua.OK -> Lua.pcall 0 multret Nothing
  err    -> return err
{-# INLINABLE dofile #-}

-- | Pushes onto the stack the field @e@ from the metatable of the
-- object at index @obj@ and returns the type of the pushed value. If
-- the object does not have a metatable, or if the metatable does not
-- have this field, pushes nothing and returns 'Lua.TypeNil'.
--
-- Wraps 'luaL_getmetafield'.
getmetafield :: StackIndex -- ^ obj
             -> Name       -- ^ e
             -> LuaE e Lua.Type
getmetafield obj (Name name) = liftLua $ \l ->
  B.useAsCString name $! fmap Lua.toType . luaL_getmetafield l obj
{-# INLINABLE getmetafield #-}

-- | Pushes onto the stack the metatable associated with name @tname@ in
-- the registry (see 'newmetatable') (@nil@ if there is no metatable
-- associated with that name). Returns the type of the pushed value.
--
-- Wraps 'luaL_getmetatable'.
getmetatable' :: Name      -- ^ tname
              -> LuaE e Lua.Type
getmetatable' (Name tname) = liftLua $ \l ->
  B.useAsCString tname $ fmap Lua.toType . luaL_getmetatable l
{-# INLINABLE getmetatable' #-}

-- | Push referenced value from the table at the given index.
getref :: LuaError e => StackIndex -> Reference -> LuaE e Lua.Type
getref idx ref' = Lua.rawgeti idx (fromIntegral (Lua.fromReference ref'))
{-# INLINABLE getref #-}

-- | Ensures that the value @t[fname]@, where @t@ is the value at index
-- @idx@, is a table, and pushes that table onto the stack. Returns True
-- if it finds a previous table there and False if it creates a new
-- table.
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
{-# INLINABLE getsubtable #-}

-- | Loads a ByteString as a Lua chunk.
--
-- This function returns the same results as @'Lua.load'@. @name@ is the
-- chunk name, used for debug information and error messages. Note that
-- @name@ is used as a C string, so it may not contain null-bytes.
--
-- Wraps 'luaL_loadbuffer'.
loadbuffer :: ByteString -- ^ Program to load
           -> Name       -- ^ chunk name
           -> LuaE e Status
loadbuffer bs (Name name) = liftLua $ \l ->
  B.useAsCStringLen bs $ \(str, len) ->
  B.useAsCString name $!
    fmap Lua.toStatus . luaL_loadbuffer l str (fromIntegral len)
{-# INLINABLE loadbuffer #-}

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
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: FilePath -- ^ filename
         -> LuaE e Status
loadfile fp = liftLua $ \l ->
  withCString fp $! fmap Lua.toStatus . luaL_loadfile l
{-# INLINABLE loadfile #-}

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
-- See
-- <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: ByteString -> LuaE e Status
loadstring s = loadbuffer s (Name s)
{-# INLINE loadstring #-}

-- | If the registry already has the key tname, returns @False@.
-- Otherwise, creates a new table to be used as a metatable for
-- userdata, adds to this new table the pair @__name = tname@, adds to
-- the registry the pair @[tname] = new table@, and returns @True@. (The
-- entry @__name@ is used by some error-reporting functions.)
--
-- In both cases pushes onto the stack the final value associated with
-- @tname@ in the registry.
--
-- The value of @tname@ is used as a C string and hence must not contain
-- null bytes.
--
-- Wraps 'luaL_newmetatable'.
newmetatable :: Name -> LuaE e Bool
newmetatable (Name tname) = liftLua $ \l ->
  Lua.fromLuaBool <$!> B.useAsCString tname (luaL_newmetatable l)
{-# INLINABLE newmetatable #-}

-- | Creates a new Lua state. It calls @lua_newstate@ with an allocator
-- based on the standard C @realloc@ function and then sets a panic
-- function (see <https://www.lua.org/manual/5.3/manual.html#4.6 §4.6>
-- of the Lua 5.3 Reference Manual) that prints an error message to the
-- standard error output in case of fatal errors.
--
-- Wraps 'hsluaL_newstate'. See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO Lua.State
newstate = hsluaL_newstate
{-# INLINE newstate #-}

-- | Creates and returns a reference, in the table at index @t@, for the
-- object at the top of the stack (and pops the object).
--
-- A reference is a unique integer key. As long as you do not manually
-- add integer keys into table @t@, @ref@ ensures the uniqueness of the
-- key it returns. You can retrieve an object referred by reference @r@
-- by calling @rawgeti t r@. Function @'unref'@ frees a reference and
-- its associated object.
--
-- If the object at the top of the stack is nil, @'ref'@ returns the
-- constant @'Lua.refnil'@. The constant @'Lua.noref'@ is guaranteed to
-- be different from any reference returned by @'ref'@.
--
-- Wraps 'luaL_ref'.
ref :: StackIndex -> LuaE e Reference
ref t = liftLua $ \l -> Lua.toReference <$> luaL_ref l t
{-# INLINABLE ref #-}

-- | If @modname@ is not already present in @package.loaded@. calls
-- function @openf@ with string @modname@ as an argument and sets the
-- call result in @package.loaded[modname]@, as if that function has
-- been called through
-- <https://www.lua.org/manual/5.3/manual.html#pdf-require require>.
--
-- If @glb@ is true, also stores the module into global @modname@.
--
-- Leaves a copy of the module on the stack.
--
-- See 'requirehs' for a version intended to be used with Haskell
-- actions.
requiref :: LuaError e
         => Name          -- ^ modname
         -> Lua.CFunction -- ^ openf
         -> Bool          -- ^ glb
         -> LuaE e ()
requiref (Name name) openf glb = liftLuaThrow $ \l status' ->
  B.useAsCString name $ \namePtr ->
    hsluaL_requiref l namePtr openf (Lua.toLuaBool glb) status'

-- | Converts any Lua value at the given index to a 'ByteString' in a
-- reasonable format. The resulting string is pushed onto the stack and
-- also returned by the function.
--
-- If the value has a metatable with a @__tostring@ field, then
-- @tolstring'@ calls the corresponding metamethod with the value as
-- argument, and uses the result of the call as its result.
--
-- Wraps 'hsluaL_tolstring'.
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
{-# INLINABLE tostring' #-}

-- | Creates and pushes a traceback of the stack L1. If a message is
-- given it is appended at the beginning of the traceback. The level
-- parameter tells at which level to start the traceback.
--
-- Wraps 'luaL_traceback'.
traceback :: Lua.State -> Maybe ByteString -> Int -> LuaE e ()
traceback l1 msg level = liftLua $ \l ->
  case msg of
    Nothing -> luaL_traceback l l1 nullPtr (fromIntegral level)
    Just msg' -> B.useAsCString msg' $ \cstr ->
      luaL_traceback l l1 cstr (fromIntegral level)
{-# INLINABLE traceback #-}

-- | Releases reference @'ref'@ from the table at index @idx@ (see
-- @'ref'@). The entry is removed from the table, so that the referred
-- object can be collected. The reference @'ref'@ is also freed to be
-- used again.
--
-- Wraps 'luaL_unref'. See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -- ^ idx
      -> Reference  -- ^ ref
      -> LuaE e ()
unref idx r = liftLua $ \l ->
  luaL_unref l idx (Lua.fromReference r)
{-# INLINABLE unref #-}

-- | Pushes onto the stack a string identifying the current position of
-- the control at level @lvl@ in the call stack. Typically this string
-- has the following format:
--
-- > chunkname:currentline:
--
-- Level 0 is the running function, level 1 is the function that called
-- the running function, etc.
--
-- This function is used to build a prefix for error messages.
where' :: Int        -- ^ lvl
       -> LuaE e ()
where' lvl = liftLua $ \l -> luaL_where l (fromIntegral lvl)
{-# INLINABLE where' #-}

--
-- Registry fields
--

-- | Key to the registry field that holds the table of loaded modules.
loaded :: Name
loaded = fromString loadedTableRegistryField

-- | Key to the registry field that holds the table of loader functions.
preload :: Name
preload = fromString preloadTableRegistryField
