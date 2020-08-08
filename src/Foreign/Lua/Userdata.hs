{-# LANGUAGE OverloadedStrings   #-}
{-|
Module      : Foreign.Lua.Userdata
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Convenience functions to convert Haskell values into Lua userdata.

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
module Foreign.Lua.Userdata
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
import Foreign.Lua.Core (Lua)
import Foreign.Lua.Core.Types (liftLua, fromLuaBool)
import Foreign.Lua.Raw.Auxiliary (luaL_testudata)
import Foreign.Lua.Raw.Userdata (hslua_newudmetatable)
import Foreign.Lua.Types.Peekable (reportValueOnFailure)

import qualified Foreign.Lua.Core as Lua
import qualified Foreign.C as C
import qualified Foreign.Ptr as Ptr
import qualified Foreign.StablePtr as StablePtr
import qualified Foreign.Storable as Storable


-- | Push data by wrapping it into a userdata object.
pushAny :: Data a
        => a
        -> Lua ()
pushAny x =
  let name = metatableName x
      pushMetatable = ensureUserdataMetatable name (return ())
  in pushAnyWithMetatable pushMetatable x

-- | Push data by wrapping it into a userdata object, using the object at the
-- top of the stack after performing the given operation as metatable.
pushAnyWithMetatable :: Lua ()       -- ^ operation to push the metatable
                     -> a            -- ^ object to push to Lua.
                     -> Lua ()
pushAnyWithMetatable mtOp x = do
  xPtr <- Lua.liftIO (StablePtr.newStablePtr x)
  udPtr <- Lua.newuserdata (Storable.sizeOf xPtr)
  Lua.liftIO $ Storable.poke (Ptr.castPtr udPtr) xPtr
  mtOp
  Lua.setmetatable (Lua.nthFromTop 2)
  return ()

-- | Push the metatable used to define the behavior of the given value in Lua.
-- The table will be created if it doesn't exist yet.
ensureUserdataMetatable :: String     -- ^ name of the registered
                                      -- metatable which should be used.
                        -> Lua ()     -- ^ set additional properties; this
                                      -- operation will be called with the newly
                                      -- created metadata table at the top of
                                      -- the stack.
                        -> Lua ()
ensureUserdataMetatable name modMt = do
  mtCreated <- liftLua $ \l ->
    fromLuaBool <$> C.withCString name (hslua_newudmetatable l)
  -- Execute additional modifications on metatable
  when mtCreated modMt

-- | Retrieve data which has been pushed with @'pushAny'@.
toAny :: Data a => Lua.StackIndex -> Lua (Maybe a)
toAny idx = toAny' undefined
 where
  toAny' :: Data a => a -> Lua (Maybe a)
  toAny' x = toAnyWithName idx (metatableName x)

-- | Retrieve data which has been pushed with @'pushAnyWithMetatable'@, where
-- *name* must is the value of the @__name@ field of the metatable.
toAnyWithName :: Lua.StackIndex
              -> String         -- ^ expected metatable name
              -> Lua (Maybe a)
toAnyWithName idx name = do
  l <- Lua.state
  udPtr <- Lua.liftIO (C.withCString name (luaL_testudata l idx))
  if udPtr == Ptr.nullPtr
    then return Nothing
    else
      fmap Just . Lua.liftIO $
      Storable.peek (Ptr.castPtr udPtr) >>= StablePtr.deRefStablePtr

-- | Retrieve Haskell data which was pushed to Lua as userdata.
peekAny :: Data a => Lua.StackIndex -> Lua a
peekAny idx = peek' undefined
 where
  peek' :: Data a => a -> Lua a
  peek' x = reportValueOnFailure (dataTypeName (dataTypeOf x)) toAny idx

-- | Return the default name for userdata to be used when wrapping an object as
-- the given type as userdata.  The argument is never evaluated.
metatableName :: Data a => a -> String
metatableName x = "HSLUA_" ++ dataTypeName (dataTypeOf x)
