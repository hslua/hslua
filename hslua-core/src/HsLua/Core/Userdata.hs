{-# LANGUAGE RankNTypes #-}
{-|
Module      : HsLua.Core.Userdata
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Convenience functions to convert Haskell values into Lua userdata.
-}
module HsLua.Core.Userdata
  ( newhsuserdata
  , newudmetatable
  , fromuserdata
  , putuserdata
  ) where

import HsLua.Core.Types (LuaE, Name (..), StackIndex, liftLua, fromLuaBool)
import Lua.Userdata
  ( hslua_fromuserdata
  , hslua_newhsuserdata
  , hslua_newudmetatable
  , hslua_putuserdata
  )
import qualified Data.ByteString as B

-- | Creates a new userdata wrapping the given Haskell object. The
-- userdata is pushed to the top of the stack.
newhsuserdata :: forall a e. a -> LuaE e ()
newhsuserdata = liftLua . flip hslua_newhsuserdata
{-# INLINABLE newhsuserdata #-}

-- | Creates and registers a new metatable for a userdata-wrapped
-- Haskell value; checks whether a metatable of that name has been
-- registered yet and uses the registered table if possible.
--
-- Using a metatable created by this functions ensures that the pointer
-- to the Haskell value will be freed when the userdata object is
-- garbage collected in Lua.
--
-- The name may not contain a nul character.
newudmetatable :: Name -> LuaE e Bool
newudmetatable (Name name) = liftLua $ \l ->
  B.useAsCString name (fmap fromLuaBool . hslua_newudmetatable l)
{-# INLINABLE newudmetatable #-}

-- | Retrieves a Haskell object from userdata at the given index. The
-- userdata /must/ have the given name.
fromuserdata :: forall a e.
                StackIndex  -- ^ stack index of userdata
             -> Name        -- ^ expected name of userdata object
             -> LuaE e (Maybe a)
fromuserdata idx (Name name) = liftLua $ \l ->
  B.useAsCString name (hslua_fromuserdata l idx)
{-# INLINABLE fromuserdata #-}

-- | Replaces the Haskell value contained in the userdata value at
-- @index@. Checks that the userdata is of type @name@ and returns
-- 'True' on success, or 'False' otherwise.
putuserdata :: forall a e.
               StackIndex   -- ^ index
            -> Name         -- ^ name
            -> a            -- ^ new value
            -> LuaE e Bool
putuserdata idx (Name name) x = liftLua $ \l ->
  B.useAsCString name $ \namePtr ->
  hslua_putuserdata l idx namePtr x
{-# INLINABLE putuserdata #-}
