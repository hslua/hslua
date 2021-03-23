{-# LANGUAGE OverloadedStrings   #-}
{-|
Module      : HsLua.Marshalling.Userdata
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Convenience functions to convert Haskell values into Lua userdata.

FIXME
The main purpose of this module is to allow fast and simple
creation of instances for @Peekable@ and @Pushable@. E.g., given
a data type Person

> data Person = Person { name :: String, age :: Int }
>    deriving (Eq, Show, Typeable, Data)

we can simply do

> instance Lua.Peekable Person where
>     safePeek = safePeekAny
>
> instance Lua.Pushable Person where
>     push = pushAny

The other functions can be used to exert more control over the userdata wrapping
and unwrapping process.
-}
module HsLua.Marshalling.Userdata
  ( pushAny
  , pushAnyWithMetatable
  , toAny
  , toAnyWithName
  , peekAny
  , ensureUserdataMetatable
  , metatableName
  ) where

import Control.Monad (when)
import Data.Data (Data, dataTypeName, dataTypeOf)
import Data.String (IsString (..))
import HsLua.Core
  ( LuaE, LuaError, Name, fromuserdata, newhsuserdata, newudmetatable
  , nth, throwTypeMismatchError )

import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8

-- | Push data by wrapping it into a userdata object.
pushAny :: Data a
        => a
        -> LuaE e ()
pushAny x =
  let name = metatableName x
      pushMetatable = ensureUserdataMetatable name (return ())
  in pushAnyWithMetatable pushMetatable x

-- | Push data by wrapping it into a userdata object, using the object at the
-- top of the stack after performing the given operation as metatable.
pushAnyWithMetatable :: LuaE e ()       -- ^ operation to push the metatable
                     -> a            -- ^ object to push to Lua.
                     -> LuaE e ()
pushAnyWithMetatable mtOp x = do
  newhsuserdata x
  mtOp
  Lua.setmetatable (nth 2)
  return ()

-- | Push the metatable used to define the behavior of the given value in Lua.
-- The table will be created if it doesn't exist yet.
ensureUserdataMetatable :: Name       -- ^ name of the registered
                                      -- metatable which should be used.
                        -> LuaE e ()     -- ^ set additional properties; this
                                      -- operation will be called with the newly
                                      -- created metadata table at the top of
                                      -- the stack.
                        -> LuaE e ()
ensureUserdataMetatable name modMt = do
  mtCreated <- newudmetatable name
  -- Execute additional modifications on metatable
  when mtCreated modMt

-- | Retrieve data which has been pushed with @'pushAny'@.
toAny :: Data a => Lua.StackIndex -> LuaE e (Maybe a)
toAny idx = toAny' undefined
 where
  toAny' :: Data a => a -> LuaE e (Maybe a)
  toAny' x = toAnyWithName idx (metatableName x)

-- | Retrieve data which has been pushed with @'pushAnyWithMetatable'@,
-- where *name* must is the value of the @__name@ field of the
-- metatable.
--
-- Alias for 'fromuserdata'.
toAnyWithName :: Lua.StackIndex
              -> Name             -- ^ expected metatable name
              -> LuaE e (Maybe a)
toAnyWithName = fromuserdata

-- | Retrieve Haskell data which was pushed to Lua as userdata.
peekAny :: (LuaError e, Data a) => Lua.StackIndex -> LuaE e a
peekAny idx = peek' undefined
 where
  peek' :: (LuaError e, Data a) => a -> LuaE e a
  peek' x = toAny idx >>= \case
    Just x' -> return x'
    Nothing -> do
      let expected = Utf8.fromString (dataTypeName (dataTypeOf x))
      throwTypeMismatchError expected idx

-- | Return the default name for userdata to be used when wrapping an object as
-- the given type as userdata.  The argument is never evaluated.
metatableName :: Data a => a -> Name
metatableName x = fromString $ "HSLUA_" ++ dataTypeName (dataTypeOf x)
