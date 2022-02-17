{-# LANGUAGE CPP #-}
{-|
Module      : Lua.Auxiliary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Raw bindings to functions and constants of the auxiliary library.
-}
module Lua.Auxiliary
  ( -- * The Auxiliary Library
    luaL_getmetafield
  , luaL_getmetatable
  , luaL_loadbuffer
  , luaL_loadfile
  , luaL_loadfilex
  , luaL_openlibs
  , luaL_newmetatable
  , luaL_ref
  , luaL_testudata
  , luaL_traceback
  , luaL_unref
  , luaL_where
    -- ** Registry fields
  , loadedTableRegistryField
  , preloadTableRegistryField
    -- ** References
  , Reference (..)
  , fromReference
  , toReference
  ) where

import Foreign.C (CChar, CInt (CInt), CSize (CSize), CString)
import Lua.Types as Lua
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Foreign.C as C

#ifdef ALLOW_UNSAFE_GC
#define SAFTY unsafe
#else
#define SAFTY safe
#endif

-- * The Auxiliary Library

-- | Key, in the registry, for table of loaded modules.
loadedTableRegistryField :: String
loadedTableRegistryField = unsafePerformIO (C.peekCString c_loaded_table)
{-# NOINLINE loadedTableRegistryField #-}

foreign import capi unsafe "lauxlib.h value LUA_LOADED_TABLE"
  c_loaded_table :: CString

-- | Key, in the registry, for table of preloaded loaders.
preloadTableRegistryField :: String
preloadTableRegistryField = unsafePerformIO (C.peekCString c_preload_table)
{-# NOINLINE preloadTableRegistryField #-}

foreign import capi unsafe "lauxlib.h value LUA_PRELOAD_TABLE"
  c_preload_table :: CString

-- | Pushes onto the stack the field @e@ from the metatable of the
-- object at index @obj@ and returns the type of the pushed value. If
-- the object does not have a metatable, or if the metatable does not
-- have this field, pushes nothing and returns
-- @'Lua.Constants.LUA_TNIL'@.
foreign import capi SAFTY "lauxlib.h luaL_getmetafield"
  luaL_getmetafield :: Lua.State
                    -> StackIndex      -- ^ obj
                    -> CString         -- ^ e
                    -> IO TypeCode

-- | Pushes onto the stack the metatable associated with name tname in
-- the registry (see @'luaL_newmetatable'@) (__nil__ if there is no
-- metatable associated with that name). Returns the type of the pushed
-- value.
foreign import capi SAFTY "lauxlib.h luaL_getmetatable"
  luaL_getmetatable :: Lua.State -> CString -> IO Lua.TypeCode

-- | Loads a buffer as a Lua chunk. This function uses @lua_load@ to
-- load the chunk in the buffer pointed to by @buff@ with size @sz@.
--
-- This function returns the same results as @lua_load@. @name@ is the
-- chunk name, used for debug information and error messages.
foreign import capi SAFTY "lauxlib.h luaL_loadbuffer"
  luaL_loadbuffer :: Lua.State
                  -> Ptr CChar         -- ^ buff
                  -> CSize             -- ^ sz
                  -> CString           -- ^ name
                  -> IO Lua.StatusCode

-- | Equivalent to luaL_loadfilex with mode equal to @NULL@.
foreign import capi SAFTY "lauxlib.h luaL_loadfile"
  luaL_loadfile :: Lua.State
                -> Ptr CChar  -- ^ filename
                -> IO Lua.StatusCode

-- | Loads a file as a Lua chunk. This function uses @lua_load@ to load
-- the chunk in the file named filename. If filename is @NULL@, then it
-- loads from the standard input. The first line in the file is ignored
-- if it starts with a @#@.
--
-- The string mode works as in function @lua_load@.
--
-- This function returns the same results as @lua_load@, but it has an
-- extra error code LUA_ERRFILE for file-related errors (e.g., it cannot
-- open or read the file).
--
-- As @lua_load@, this function only loads the chunk; it does not run
-- it.
foreign import capi SAFTY "lauxlib.h luaL_loadfilex"
  luaL_loadfilex :: Lua.State
                 -> Ptr CChar  -- ^ filename
                 -> Ptr CChar  -- ^ mode
                 -> IO Lua.StatusCode

-- | If the registry already has the key @tname@, returns @0@.
-- Otherwise, creates a new table to be used as a metatable for
-- userdata, adds to this new table the pair @__name = tname@, adds to
-- the registry the pair @[tname] = new table@, and returns @1@. (The
-- entry @__name@ is used by some error-reporting functions.)
--
-- In both cases pushes onto the stack the final value associated with
-- @tname@ in the registry.
foreign import capi SAFTY "lauxlib.h luaL_newmetatable"
  luaL_newmetatable :: Lua.State -> CString {- ^ tname -} -> IO Lua.LuaBool

-- | Opens all standard Lua libraries into the given state.
--
-- <https://www.lua.org/manual/5.4/manual.html#luaL_openlibs>
foreign import capi unsafe "lualib.h luaL_openlibs"
  luaL_openlibs :: Lua.State -> IO ()

-- | Creates and returns a reference, in the table at index @t@, for the
-- object at the top of the stack (and pops the object).
--
-- A reference is a unique integer key. As long as you do not manually
-- add integer keys into table @t@, @luaL_ref@ ensures the uniqueness of
-- the key it returns. You can retrieve an object referred by reference
-- @r@ by calling @lua_rawgeti l t r@. Function @'luaL_unref'@ frees a
-- reference and its associated object.
--
-- If the object at the top of the stack is nil, @luaL_ref@ returns the
-- constant @'Lua.Constants.LUA_REFNIL'@. The constant
-- @'Lua.Constants.LUA_NOREF'@ is guaranteed to be different
-- from any reference returned by @luaL_ref@.
foreign import capi SAFTY "lauxlib.h luaL_ref"
  luaL_ref :: Lua.State -> StackIndex {- ^ t -} -> IO CInt

-- | Creates and pushes a traceback of the stack @l1@. If @msg@ is not
-- @NULL@ it is appended at the beginning of the traceback. The level
-- parameter tells at which level to start the traceback.
foreign import capi SAFTY "lauxlib.h luaL_traceback"
  luaL_traceback :: Lua.State  -- ^ l
                 -> Lua.State  -- ^ l1
                 -> CString    -- ^ msg
                 -> CInt       -- ^ level
                 -> IO ()

-- | Releases reference @ref@ from the table at index @t@ (see
-- @'luaL_ref'@). The entry is removed from the table, so that the
-- referred object can be collected. The reference @ref@ is also freed
-- to be used again.
foreign import capi SAFTY "lauxlib.h luaL_unref"
  luaL_unref :: Lua.State -> StackIndex {- ^ t -} -> CInt {- ^ ref -} -> IO ()

-- | Checks whether the function argument @arg@ is a userdata of the
-- type @tname@ (see @'luaL_newmetatable'@) and returns the userdata
-- address (see @'Lua.lua_touserdata'@). Returns @NULL@ if the
-- test fails.
--
-- <https://www.lua.org/manual/5.4/manual.html#luaL_testudata>
foreign import capi SAFTY "lauxlib.h luaL_testudata"
  luaL_testudata :: Lua.State   -- ^ l
                 -> StackIndex  -- ^ arg
                 -> CString     -- ^ tname
                 -> IO (Ptr ())

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
foreign import capi SAFTY "lauxlib.h luaL_where"
  luaL_where :: Lua.State -- ^ l
             -> CInt      -- ^ lvl
             -> IO ()

--
-- References
--

-- | Reference to a stored value.
data Reference =
    Reference CInt -- ^ Reference to a stored value
  | RefNil         -- ^ Reference to a nil value
  deriving (Eq, Show)

foreign import capi SAFTY "lauxlib.h value LUA_REFNIL"
  refnil :: CInt

-- | Convert a reference to its C representation.
fromReference :: Reference -> CInt
fromReference = \case
  Reference x -> x
  RefNil      -> refnil

-- | Create a reference from its C representation.
toReference :: CInt -> Reference
toReference x =
  if x == refnil
  then RefNil
  else Reference x
