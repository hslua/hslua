{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
  , pushIterator
  ) where

import Control.Monad (when, void)
import Data.Data (Data, dataTypeName, dataTypeOf)
import Data.String (IsString (..))
import HsLua.Core as Lua

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

-- | Pushes three values to the stack that can be used in a generic for
-- loop to lazily iterate over all values in the list. Keeps the
-- remaining list in a userdata state.
pushIterator :: forall a e. LuaError e
             => (a -> LuaE e NumResults)  -- ^ the values to push
             -> [a]                       -- ^ list to iterate over lazily
             -> LuaE e NumResults
pushIterator pushValues xs = do
  -- push initial state
  pushHaskellFunction nextItem
  pushInitialState
  pushnil
  return (NumResults 3)
  where
    nextItem :: LuaE e NumResults
    nextItem = do
      props <- fromuserdata @[a] (nthBottom 1) statename
      case props of
        Nothing -> failLua
          "Error in iterator: could not retrieve iterator state."
        Just [] -> 2 <$ (pushnil *> pushnil)  -- end loop
        Just (y:ys) -> do
          success <- putuserdata @[a] (nthBottom 1) statename ys
          if not success
            then failLua "Error in iterator: could not update iterator state."
            else pushValues y

    statename :: Name
    statename = "HsLua iterator state"

    pushInitialState :: LuaE e ()
    pushInitialState = do
      newhsuserdata @[a] xs
      void (newudmetatable statename)
      setmetatable (nth 2)
