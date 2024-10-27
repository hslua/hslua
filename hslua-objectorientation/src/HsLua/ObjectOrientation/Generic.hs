{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-|
Module      : HsLua.ObjectOrientation.Generic
Copyright   : © 2021-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

This module provides types and functions to use Haskell values as
userdata objects in Lua. These objects wrap a Haskell value and provide
methods and properties to interact with the Haskell value.

The terminology in this module refers to the userdata values as /UD
objects/, and to their type as /UD type/.
-}
module HsLua.ObjectOrientation.Generic
  ( UDTypeWithList (..)
    -- * Defining types
  , deftypeGeneric'
    -- ** Methods
  , methodGeneric
    -- ** Properties
  , property
  , property'
  , possibleProperty
  , possibleProperty'
  , readonly
  , readonly'
    -- ** Aliases
  , alias
    -- * Marshaling
  , peekUDGeneric
  , pushUDGeneric
  , initTypeGeneric
    -- * Type docs
  , udDocs
  , udTypeSpec
    -- * Helper types for building
  , Member
  , Property (..)
  , Operation (..)
  , ListSpec
  , Possible (..)
  , Alias
  , AliasIndex (..)
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import Foreign.Ptr (FunPtr)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation.ListType
import HsLua.ObjectOrientation.Operation
import HsLua.Typing ( TypeDocs (..), TypeSpec (..), anyType, userdataType )
import qualified Data.Map.Strict as Map
import qualified HsLua.Core.Unsafe as Unsafe
import qualified HsLua.Core.Utf8 as Utf8

-- | A userdata type, capturing the behavior of Lua objects that wrap
-- Haskell values. The type name must be unique; once the type has been
-- used to push or retrieve a value, the behavior can no longer be
-- modified through this type.
--
-- This type includes methods to define how the object should behave as
-- a read-only list of type @itemtype@.
data UDTypeWithList e fn a itemtype = UDTypeWithList
  { udName          :: Name
  , udOperations    :: [(Operation, fn)]
  , udProperties    :: Map Name (Property e a)
  , udMethods       :: Map Name fn
  , udAliases       :: Map AliasIndex Alias
  , udListSpec      :: Maybe (ListSpec e a itemtype)
  , udFnPusher      :: fn -> LuaE e ()
  }

-- | Defines a new type that could also be treated as a list; defines
-- the behavior of objects in Lua. Note that the type name must be
-- unique.
deftypeGeneric' :: Pusher e fn          -- ^ function pusher
                -> Name                 -- ^ type name
                -> [(Operation, fn)]    -- ^ operations
                -> [Member e fn a]      -- ^ methods
                -> Maybe (ListSpec e a itemtype)  -- ^ list access
                -> UDTypeWithList e fn a itemtype
deftypeGeneric' pushFunction name ops members mbListSpec = UDTypeWithList
  { udName          = name
  , udOperations    = ops
  , udProperties    = Map.fromList $ mapMaybe mbproperties members
  , udMethods       = Map.fromList $ mapMaybe mbmethods members
  , udAliases       = Map.fromList $ mapMaybe mbaliases members
  , udListSpec      = mbListSpec
  , udFnPusher      = pushFunction
  }
  where
    mbproperties = \case
      MemberProperty n p -> Just (n, p)
      _ -> Nothing
    mbmethods = \case
      MemberMethod n m -> Just (n, m)
      _ -> Nothing
    mbaliases = \case
      MemberAlias n a -> Just (n, a)
      _ -> Nothing

-- | A read- and writable property on a UD object.
data Property e a = Property
  { propertyGet :: a -> LuaE e NumResults
  , propertySet :: Maybe (StackIndex -> a -> LuaE e a)
  , propertyDescription :: Text
  , propertyType :: TypeSpec
  }

-- | Alias for a different property of this or of a nested object.
type Alias = [AliasIndex]

-- | Index types allowed in aliases (strings and integers)
data AliasIndex
  = StringIndex Name
  | IntegerIndex Lua.Integer
  deriving (Eq, Ord)

instance IsString AliasIndex where
  fromString = StringIndex . fromString

-- | A type member, either a method or a variable.
data Member e fn a
  = MemberProperty Name (Property e a)
  | MemberMethod Name fn
  | MemberAlias AliasIndex Alias

-- | Use a documented function as an object method.
methodGeneric :: Name -> fn -> Member e fn a
methodGeneric = MemberMethod

-- | A property or method which may be available in some instances but
-- not in others.
data Possible a
  = Actual a
  | Absent

-- | Declares a new read- and writable typed property.
property' :: LuaError e
          => Name                       -- ^ property name
          -> TypeSpec                   -- ^ property type
          -> Text                       -- ^ property description
          -> (Pusher e b, a -> b)       -- ^ how to get the property value
          -> (Peeker e b, a -> b -> a)  -- ^ how to set a new property value
          -> Member e fn a
property' name typespec desc (push, get) (peek, set) =
  possibleProperty' name typespec desc
    (push, Actual . get)
    (peek, \a b -> Actual (set a b))

-- | Declares a new read- and writable property.
property :: LuaError e
         => Name                       -- ^ property name
         -> Text                       -- ^ property description
         -> (Pusher e b, a -> b)       -- ^ how to get the property value
         -> (Peeker e b, a -> b -> a)  -- ^ how to set a new property value
         -> Member e fn a
property name desc (push, get) (peek, set) =
  possibleProperty name desc
    (push, Actual . get)
    (peek, \a b -> Actual (set a b))

-- | Declares a new read- and writable property which is not always
-- available.
possibleProperty :: LuaError e
  => Name                               -- ^ property name
  -> Text                               -- ^ property description
  -> (Pusher e b, a -> Possible b)      -- ^ how to get the property value
  -> (Peeker e b, a -> b -> Possible a) -- ^ how to set a new property value
  -> Member e fn a
possibleProperty name = possibleProperty' name anyType

-- | Declares a new read- and writable property which is not always
-- available.
possibleProperty' :: LuaError e
  => Name                               -- ^ property name
  -> TypeSpec                           -- ^ type of the property value
  -> Text                               -- ^ property description
  -> (Pusher e b, a -> Possible b)      -- ^ how to get the property value
  -> (Peeker e b, a -> b -> Possible a) -- ^ how to set a new property value
  -> Member e fn a
possibleProperty' name typespec desc (push, get) (peek, set) =
  MemberProperty name $
  Property
  { propertyGet = \x -> do
      case get x of
        Actual y -> NumResults 1 <$ push y
        Absent   -> return (NumResults 0)
  , propertySet = Just $ \idx x -> do
      value  <- forcePeek $ peek idx
      case set x value of
        Actual y -> return y
        Absent   -> failLua $ "Trying to set unavailable property "
                            <> Utf8.toString (fromName name)
                            <> "."
  , propertyType = typespec
  , propertyDescription = desc
  }

-- | Creates a read-only object property. Attempts to set the value will
-- cause an error.
readonly' :: Name                 -- ^ property name
          -> TypeSpec             -- ^ property type
          -> Text                 -- ^ property description
          -> (Pusher e b, a -> b) -- ^ how to get the property value
          -> Member e fn a
readonly' name typespec desc (push, get) = MemberProperty name $
  Property
  { propertyGet = \x -> do
      push $ get x
      return (NumResults 1)
  , propertySet = Nothing
  , propertyType = typespec
  , propertyDescription = desc
  }

-- | Creates a read-only object property. Attempts to set the value will
-- cause an error.
readonly :: Name                 -- ^ property name
         -> Text                 -- ^ property description
         -> (Pusher e b, a -> b) -- ^ how to get the property value
         -> Member e fn a
readonly name = readonly' name anyType

-- | Define an alias for another, possibly nested, property.
alias :: AliasIndex    -- ^ property alias
      -> Text          -- ^ description
      -> [AliasIndex]  -- ^ sequence of nested properties
      -> Member e fn a
alias name _desc = MemberAlias name

-- | Ensures that the type has been fully initialized, i.e., that all
-- metatables have been created and stored in the registry. Returns the
-- name of the initialized type.
--
-- The @hook@ can be used to perform additional setup operations. The
-- function is called as the last step after the type metatable has been
-- initialized: the fully initialized metatable will be at the top of
-- the stack at that point. Note that the hook will /not/ be called if
-- the type's metatable already existed before this function was
-- invoked.
initTypeGeneric :: LuaError e
                => (UDTypeWithList e fn a itemtype -> LuaE e ())
                -> UDTypeWithList e fn a itemtype
                -> LuaE e Name
initTypeGeneric hook ty = do
  pushUDMetatable hook ty
  pop 1
  return (udName ty)

-- | Pushes the metatable for the given type to the Lua stack. Creates
-- the new table afresh on the first time it is needed, and retrieves it
-- from the registry after that.
--
--
-- A @hook@ can be used to perform additional setup operations. The
-- function is called as the last step after the type metatable has been
-- initialized: the fully initialized metatable will be at the top of
-- the stack at that point. Note that the hook will /not/ be called if
-- the type's metatable already existed before this function was
-- invoked.
pushUDMetatable :: LuaError e
  => (UDTypeWithList e fn a itemtype -> LuaE e ())  -- ^ @hook@
  -> UDTypeWithList e fn a itemtype
  -> LuaE e ()
pushUDMetatable hook ty = do
  created <- newudmetatable (udName ty)
  when created $ do
    add (metamethodName Index)    $ pushcfunction hslua_udindex_ptr
    add (metamethodName Newindex) $ pushcfunction hslua_udnewindex_ptr
    add (metamethodName Pairs)    $ pushHaskellFunction (pairsFunction ty)
    forM_ (udOperations ty) $ \(op, f) -> do
      add (metamethodName op) $ udFnPusher ty f
    add "getters" $ pushGetters ty
    add "setters" $ pushSetters ty
    add "methods" $ pushMethods ty
    add "aliases" $ pushAliases ty
    case udListSpec ty of
      Nothing -> pure ()
      Just ((pushItem, _), _) -> do
        add "lazylisteval" $ pushHaskellFunction (lazylisteval pushItem)
    hook ty
  where
    add :: LuaError e => Name -> LuaE e () -> LuaE e ()
    add name op = do
      pushName name
      op
      rawset (nth 3)

-- | Retrieves a key from a Haskell-data holding userdata value.
--
-- Does the following, in order, and returns the first non-nil result:
--
--   - Checks the userdata's uservalue table for the given key;
--
--   - Looks up a @getter@ for the key and calls it with the userdata
--     and key as arguments;
--
--   - Looks up the key in the table in the @methods@ metafield.
foreign import ccall "hslobj.c &hslua_udindex"
  hslua_udindex_ptr :: FunPtr (State -> IO NumResults)

-- | Sets a new value in the userdata caching table via a setter
-- functions.
--
-- The actual assignment is performed by a setter function stored in the
-- @setter@ metafield. Throws an error if no setter function can be
-- found.
foreign import ccall "hslobj.c &hslua_udnewindex"
  hslua_udnewindex_ptr :: FunPtr (State -> IO NumResults)

-- | Sets a value in the userdata's caching table (uservalue). Takes the
-- same arguments as a @__newindex@ function.
foreign import ccall "hslobj.c &hslua_udsetter"
  hslua_udsetter_ptr :: FunPtr (State -> IO NumResults)

-- | Throws an error nothing that the given key is read-only.
foreign import ccall "hslobj.c &hslua_udreadonly"
  hslua_udreadonly_ptr :: FunPtr (State -> IO NumResults)

-- | Pushes the metatable's @getters@ field table.
pushGetters :: LuaError e => UDTypeWithList e fn a itemtype -> LuaE e ()
pushGetters ty = do
  newtable
  void $ flip Map.traverseWithKey (udProperties ty) $ \name prop -> do
    pushName name
    pushHaskellFunction $ forcePeek (peekUDGeneric ty 1) >>= propertyGet prop
    rawset (nth 3)

-- | Pushes the metatable's @setters@ field table.
pushSetters :: LuaError e => UDTypeWithList e fn a itemtype -> LuaE e ()
pushSetters ty = do
  newtable
  void $ flip Map.traverseWithKey (udProperties ty) $ \name prop -> do
    pushName name
    pushcfunction $ case propertySet prop of
      Just _  -> hslua_udsetter_ptr
      Nothing -> hslua_udreadonly_ptr
    rawset (nth 3)

-- | Pushes the metatable's @methods@ field table.
pushMethods :: LuaError e => UDTypeWithList e fn a itemtype -> LuaE e ()
pushMethods ty = do
  newtable
  void $ flip Map.traverseWithKey (udMethods ty) $ \name fn -> do
    pushName name
    udFnPusher ty fn
    rawset (nth 3)

pushAliases :: LuaError e => UDTypeWithList e fn a itemtype -> LuaE e ()
pushAliases ty = do
  newtable
  void $ flip Map.traverseWithKey (udAliases ty) $ \name propSeq -> do
    pushAliasIndex name
    pushList pushAliasIndex propSeq
    rawset (nth 3)

pushAliasIndex :: Pusher e AliasIndex
pushAliasIndex = \case
  StringIndex name -> pushName name
  IntegerIndex n   -> pushIntegral n

-- | Pushes the function used to iterate over the object's key-value
-- pairs in a generic *for* loop.
pairsFunction :: forall e fn a itemtype. LuaError e
              => UDTypeWithList e fn a itemtype -> LuaE e NumResults
pairsFunction ty = do
  obj <- forcePeek $ peekUDGeneric ty (nthBottom 1)
  let pushMember = \case
        MemberProperty name prop -> do
          pushName name
          getresults <- propertyGet prop obj
          if getresults == 0
            then 0 <$ pop 1  -- property is absent, don't push anything
            else return $ getresults + 1
        MemberMethod name f -> do
          pushName name
          udFnPusher ty f
          return 2
        MemberAlias{} -> fail "aliases are not full properties"
  pushIterator pushMember $
    map (uncurry MemberProperty) (Map.toAscList (udProperties ty)) ++
    map (uncurry MemberMethod) (Map.toAscList (udMethods ty))

-- | Pushes a userdata value of the given type.
pushUDGeneric :: LuaError e
  => (UDTypeWithList e fn a itemtype -> LuaE e ()) -- ^ push docs
  -> UDTypeWithList e fn a itemtype                -- ^ userdata type
  -> a                                             -- ^ value to push
  -> LuaE e ()
pushUDGeneric pushDocs ty x = do
  newhsuserdatauv x 1
  pushUDMetatable pushDocs ty
  setmetatable (nth 2)
  -- add list as value in caching table
  case udListSpec ty of
    Nothing -> pure ()
    Just ((_, toList), _) -> do
      newtable
      pushName "__lazylist"
      newhsuserdatauv (toList x) 1
      void (newudmetatable lazyListStateName)
      setmetatable (nth 2)
      rawset (nth 3)
      void (setiuservalue (nth 2) 1)

-- | Retrieves a userdata value of the given type.
peekUDGeneric :: LuaError e => UDTypeWithList e fn a itemtype -> Peeker e a
peekUDGeneric ty idx = do
  let name = udName ty
  x <- reportValueOnFailure name (`fromuserdata` name) idx
  (`lastly` pop 1) $ liftLua (getiuservalue idx 1) >>= \case
    TypeTable -> do
      -- set list
      xWithList <- maybe pure setList (udListSpec ty) x
      liftLua $ do
        pushnil
        setProperties (udProperties ty) xWithList
    _ -> return x

-- | Retrieves object properties from a uservalue table and sets them on
-- the given value. Expects the uservalue table at the top of the stack.
setProperties :: LuaError e => Map Name (Property e a) -> a -> LuaE e a
setProperties props x = do
  hasNext <- Unsafe.next (nth 2)
  if not hasNext
    then return x
    else ltype (nth 2) >>= \case
      TypeString -> do
        propName <- forcePeek $ peekName (nth 2)
        case Map.lookup propName props >>= propertySet of
          Nothing -> pop 1 *> setProperties props x
          Just setter -> do
            x' <- setter top x
            pop 1
            setProperties props x'
      _ -> x <$ pop 1

--
-- Typing
--

-- | Returns documentation for this type.
udDocs :: UDTypeWithList e fn a itemtype
       -> TypeDocs
udDocs ty = TypeDocs
  { typeDescription = mempty
  , typeSpec = userdataType
  , typeRegistry = Just (udName ty)
  }

-- | Type specifier for a UDType
udTypeSpec :: UDTypeWithList e fn a itemtype
           -> TypeSpec
udTypeSpec = NamedType . udName
