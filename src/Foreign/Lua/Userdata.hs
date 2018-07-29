{-# LANGUAGE OverloadedStrings   #-}
{-|
Module      : Foreign.Lua.Userdata
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Convenience functions to convert Haskell values into Lua userdata.

The main purpose of this module is to allow fast and simple creation of
instances for @'Peekable'@ and @'Pushable'@. E.g., given a data type Person

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
  , safePeekAny
  , ensureUserdataMetatable
  , metatableName
  ) where

-- import Control.Applicative (empty)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Data (Data, dataTypeName, dataTypeOf)
import Foreign.Lua.Core (Lua)
import Foreign.Lua.Types.Peekable (Result, reportValueOnFailure)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
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
ensureUserdataMetatable :: ByteString -- ^ name of the registered
                                      -- metatable which should be used.
                        -> Lua ()     -- ^ set additional properties; this
                                      -- operation will be called with the newly
                                      -- created metadata table at the top of
                                      -- the stack.
                        -> Lua ()
ensureUserdataMetatable name modMt = do
  mtCreated <- Lua.newmetatable name
  when mtCreated $ do
    -- Prevent accessing or changing the metatable with
    -- getmetatable/setmetatable.
    Lua.pushboolean True
    Lua.setfield (Lua.nthFromTop 2) "__metatable"
    -- Mark objects for finalization when collecting garbage.
    Lua.pushcfunction hslua_userdata_gc_ptr
    Lua.setfield (Lua.nthFromTop 2) "__gc"
    -- Execute additional modifications on metatable
    modMt

-- | Retrieve data which has been pushed with @'pushAny'@.
toAny :: Data a => Lua.StackIndex -> Lua (Maybe a)
toAny idx = toAny' undefined
 where
  toAny' :: Data a => a -> Lua (Maybe a)
  toAny' x = toAnyWithName idx (metatableName x)

-- | Retrieve data which has been pushed with @'pushAnyWithMetatable'@, where
-- *name* must is the value of the @__name@ field of the metatable.
toAnyWithName :: Lua.StackIndex
              -> ByteString     -- ^ expected metatable name
              -> Lua (Maybe a)
toAnyWithName idx name = do
  l <- Lua.state
  udPtr <- Lua.liftIO (B.useAsCString name (luaL_testudata l idx))
  if udPtr == Ptr.nullPtr
    then return Nothing
    else
      fmap Just . Lua.liftIO $
      Storable.peek (Ptr.castPtr udPtr) >>= StablePtr.deRefStablePtr

safePeekAny :: Data a => Lua.StackIndex -> Lua (Result a)
safePeekAny idx = peek' undefined
 where
  peek' :: Data a => a -> Lua (Result a)
  peek' x = reportValueOnFailure (Char8.pack (dataTypeName (dataTypeOf x)))
                                 toAny
                                 idx

-- | Return the default name for userdata to be used when wrapping an object as
-- the given type as userdata.  The argument is never evaluated.
metatableName :: Data a => a -> ByteString
metatableName x = Char8.pack ("HSLUA_" ++ dataTypeName (dataTypeOf x))

-- | Function to free the stable pointer in a userdata, ensuring the Haskell
-- value can be garbage collected. This function does not call back into
-- Haskell, making is safe to call even from functions imported as unsafe.
foreign import ccall "&hslua_userdata_gc"
  hslua_userdata_gc_ptr :: Lua.CFunction

-- | See
-- <https://www.lua.org/manual/5.3/manual.html#luaL_testudata luaL_testudata>
foreign import ccall "luaL_testudata"
  luaL_testudata :: Lua.State -> Lua.StackIndex -> C.CString -> IO (Ptr.Ptr ())
