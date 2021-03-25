{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.UDType
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

This module provides types and functions to use Haskell values as
userdata objects in Lua. These objects wrap a Haskell value and provide
methods and properties to interact with the Haskell value.

The terminology in this module refers to the userdata values as /UD
objects/, and to their type as /UD type/.

Note that the values returned by the properties are /copies/ of the
Haskell values; modifying them will not change the underlying Haskell
values.
-}
module HsLua.Packaging.UDType
  ( UDType (..)
  , deftype
  , method
  , property
  , operation
  , peekUD
  , pushUD
  , udparam
    -- * Helper types for building
  , Member
  , Property
  , Operation
  ) where

import Control.Monad.Except
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Text (Text)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging.Function
import HsLua.Packaging.Operation
import qualified Data.Map as Map
import qualified HsLua.Core.Utf8 as Utf8

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

-- | A userdata type, capturing the behavior of Lua objects that wrap
-- Haskell values. The type name must be unique; once the type has been
-- used to push or retrieve a value, the behavior can no longer be
-- modified through this type.
data UDType e a = UDType
  { udName          :: Name
  , udOperations    :: [(Operation, DocumentedFunction e)]
  , udProperties    :: Map Name (Property e a)
  , udMethods       :: Map Name (DocumentedFunction e)
  }

-- | Defines a new type, defining the behavior of objects in Lua.
deftype :: Name -> [Member e a] -> UDType e a
deftype name members = UDType
  { udName          = name
  , udOperations    = mapMaybe mboperations members
  , udProperties    = Map.fromList $ mapMaybe mbproperties members
  , udMethods       = Map.fromList $ mapMaybe mbmethods members
  }
  where
    mbproperties = \case
      MemberProperty n p -> Just (n, p)
      _ -> Nothing
    mboperations = \case
      MemberOperation o f -> Just (o, f)
      _ -> Nothing
    mbmethods = \case
      MemberMethod n m -> Just (n, m)
      _ -> Nothing

-- | A read- and writable property on a UD object.
data Property e a = Property
  { propertyGet :: a -> LuaE e NumResults
  , propertySet :: StackIndex -> a -> LuaE e a
  , propertyDescription :: Text
  }

-- | A type member, either a method or a variable.
data Member e a
  = MemberProperty Name (Property e a)
  | MemberMethod Name (DocumentedFunction e)
  | MemberOperation Operation (DocumentedFunction e)

-- | Use a documented function as an object method.
method :: DocumentedFunction e -> Member e a
method f = MemberMethod (functionName f) f

-- | Declares a new read- and writable property.
property :: LuaError e
         => Name                       -- ^ property name
         -> Text                       -- ^ property description
         -> (Pusher e b, a -> b)       -- ^ how to get the property value
         -> (Peeker e b, a -> b -> a)  -- ^ how to set a new property value
         -> Member e a
property name desc (push, get) (peek, set) = MemberProperty name $
  Property
  { propertyGet = \x -> do
      push $ get x
      return (NumResults 1)
  , propertySet = \idx x -> do
      value  <- peek idx >>= force
      return $ set x value
  , propertyDescription = desc
  }

-- | Declares a new object operation from a documented function.
operation :: Operation             -- ^ the kind of operation
          -> DocumentedFunction e  -- ^ function used to perform the operation
          -> Member e a
operation op f = MemberOperation op $ setName (metamethodName op) f

-- | Pushes the metatable for the given type to the Lua stack. Creates
-- the new table afresh on the first time it is needed, and retrieves it
-- from the registry after that.
pushUDMetatable :: LuaError e => UDType e a -> LuaE e ()
pushUDMetatable ty = do
  created <- newudmetatable (udName ty)
  when created $ do
    pushName (metamethodName Index)
    pushIndexFunction ty
    rawset (nth 3)
    pushName (metamethodName Newindex)
    pushNewindexFunction ty
    rawset (nth 3)
    forM_ (udOperations ty) $ \(op, f) -> do
      pushName (metamethodName op)
      pushDocumentedFunction f
      rawset (nth 3)

-- | Pushes the function used to access object properties and methods.
-- This is expected to be used with the /Index/ operation.
pushIndexFunction :: LuaError e => UDType e a -> LuaE e ()
pushIndexFunction ty = pushHaskellFunction $ do
  x    <- peekUD ty (nthBottom 1) >>= force
  name <- peekName (nthBottom 2) >>= force
  case Map.lookup name (udProperties ty) of
    Just p -> propertyGet p x
    Nothing -> case Map.lookup name (udMethods ty) of
                 Just m -> 1 <$ pushDocumentedFunction m
                 Nothing -> failLua $
                            "no key " ++ Utf8.toString (fromName name)

-- | Pushes the function used to modify object properties.
-- This is expected to be used with the /Newindex/ operation.
pushNewindexFunction :: LuaError e => UDType e a -> LuaE e ()
pushNewindexFunction ty = pushHaskellFunction $ do
  x     <- peekUD ty (nthBottom 1) >>= force
  name  <- peekName (nthBottom 2) >>= force
  case Map.lookup name (udProperties ty) of
    Just p -> do
      newx <- propertySet p (nthBottom 3) x
      success <- putuserdata (nthBottom 1) (udName ty) newx
      if success
        then return (NumResults 0)
        else failLua "Could not set userdata value."
    Nothing -> failLua $ "no key " ++ Utf8.toString (fromName name)

-- | Pushes a userdata value of the given type.
pushUD :: LuaError e => UDType e a -> a -> LuaE e ()
pushUD ty x = do
  newhsuserdata x
  pushUDMetatable ty
  setmetatable (nth 2)

-- | Retrieves a userdata value of the given type.
peekUD :: LuaError e => UDType e a -> Peeker e a
peekUD ty = do
  let name = udName ty
  reportValueOnFailure name (`fromuserdata` name)

-- | Defines a function parameter that takes the given type.
udparam :: LuaError e
        => UDType e a      -- ^ expected type
        -> Text            -- ^ parameter name
        -> Text            -- ^ parameter description
        -> Parameter e a
udparam ty = parameter (peekUD ty) (Utf8.toText . fromName $ udName ty)
