{-|
Module      : HsLua.Core.Userdata
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
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
  ) where

import Data.ByteString (ByteString)
import HsLua.Core.Types (LuaE, StackIndex, liftLua, fromLuaBool)
import Lua.Userdata
  ( hslua_fromuserdata
  , hslua_newhsuserdata
  , hslua_newudmetatable
  )

import qualified Data.ByteString as B

-- | Creates a new userdata wrapping the given Haskell object. The
-- userdata is pushed to the top of the stack.
newhsuserdata :: a -> LuaE e ()
newhsuserdata = liftLua . flip hslua_newhsuserdata

-- | Creates and registers a new metatable for a userdata-wrapped
-- Haskell value; checks whether a metatable of that name has been
-- registered yet and uses the registered table if possible.
--
-- Using a metatable created by this functions ensures that the pointer
-- to the Haskell value will be freed when the userdata object is
-- garbage collected in Lua.
--
-- The name may not contain a nul character.
newudmetatable :: ByteString -> LuaE e Bool
newudmetatable name = liftLua $ \l ->
  B.useAsCString name (fmap fromLuaBool . hslua_newudmetatable l)

-- | Retrieves a Haskell object from userdata at the given index. The
-- userdata /must/ have the given name.
--
-- The name may not contain a nul character.
fromuserdata :: StackIndex  -- ^ stack index of userdata
             -> ByteString  -- ^ expected name of userdata object
             -> LuaE e (Maybe a)
fromuserdata idx name = liftLua $ \l ->
  B.useAsCString name (hslua_fromuserdata l idx)
