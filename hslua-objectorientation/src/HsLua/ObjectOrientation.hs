{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.ObjectOrientation
Copyright   : © 2021-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

This module provides types and functions to use Haskell values as
userdata objects in Lua. These objects wrap a Haskell value and provide
methods and properties to interact with the Haskell value.

The terminology in this module refers to the userdata values as /UD
objects/, and to their type as /UD type/.
-}
module HsLua.ObjectOrientation
  ( UDType
  , UDTypeWithList (..)
  , deftypeGeneric
  , deftypeGeneric'
  , methodGeneric
  , property
  , possibleProperty
  , readonly
  , alias
  , peekUD
  , pushUD
    -- * Helper types for building
  , Member
  , Property
  , Operation (..)
  , ListSpec
  , Possible (..)
  , Alias
  , AliasIndex (..)
  ) where

import Control.Monad ((<$!>), forM_, void, when)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void (Void)
import Foreign.Ptr (FunPtr)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation.Operation
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

-- | Pair of pairs, describing how a type can be used as a Lua list. The
-- first pair describes how to push the list items, and how the list is
-- extracted from the type; the second pair contains a method to
-- retrieve list items, and defines how the list is used to create an
-- updated value.
type ListSpec e a itemtype =
  ( (Pusher e itemtype, a -> [itemtype])
  , (Peeker e itemtype, a -> [itemtype] -> a)
  )

-- | A userdata type, capturing the behavior of Lua objects that wrap
-- Haskell values. The type name must be unique; once the type has been
-- used to push or retrieve a value, the behavior can no longer be
-- modified through this type.
type UDType e fn a = UDTypeWithList e fn a Void

-- | Defines a new type, defining the behavior of objects in Lua.
-- Note that the type name must be unique.
deftypeGeneric :: Pusher e fn           -- ^ function pusher
               -> Name                  -- ^ type name
               -> [(Operation, fn)]     -- ^ operations
               -> [Member e fn a]       -- ^ methods
               -> UDType e fn a
deftypeGeneric pushFunction name ops members =
  deftypeGeneric' pushFunction name ops members Nothing

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
possibleProperty name desc (push, get) (peek, set) = MemberProperty name $
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
  , propertyDescription = desc
  }

-- | Creates a read-only object property. Attempts to set the value will
-- cause an error.
readonly :: Name                 -- ^ property name
         -> Text                 -- ^ property description
         -> (Pusher e b, a -> b) -- ^ how to get the property value
         -> Member e fn a
readonly name desc (push, get) = MemberProperty name $
  Property
  { propertyGet = \x -> do
      push $ get x
      return (NumResults 1)
  , propertySet = Nothing
  , propertyDescription = desc
  }

-- | Define an alias for another, possibly nested, property.
alias :: AliasIndex    -- ^ property alias
      -> Text          -- ^ description
      -> [AliasIndex]  -- ^ sequence of nested properties
      -> Member e fn a
alias name _desc = MemberAlias name

-- | Pushes the metatable for the given type to the Lua stack. Creates
-- the new table afresh on the first time it is needed, and retrieves it
-- from the registry after that.
pushUDMetatable :: LuaError e => UDTypeWithList e fn a itemtype -> LuaE e ()
pushUDMetatable ty = do
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
    pushHaskellFunction $ forcePeek (peekUD ty 1) >>= propertyGet prop
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
  obj <- forcePeek $ peekUD ty (nthBottom 1)
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

-- | Evaluate part of a lazy list. Takes the following arguments, in
-- this order:
--
-- 1. userdata wrapping the unevalled part of the lazy list
-- 2. index of the last evaluated element
-- 3. index of the requested element
-- 4. the caching table
lazylisteval :: forall itemtype e. LuaError e
             => Pusher e itemtype -> LuaE e NumResults
lazylisteval pushItem = do
  munevaled <- fromuserdata @[itemtype] (nthBottom 1) lazyListStateName
  mcurindex <- tointeger (nthBottom 2)
  mnewindex <- tointeger (nthBottom 3)
  case (munevaled, mcurindex, mnewindex) of
    (Just unevaled, Just curindex, Just newindex) -> do
      let numElems = fromIntegral $ max (newindex - curindex) 0
          (as, rest) = splitAt numElems unevaled
      if null rest
        then do
          -- no more elements in list; unset variable
          pushName "__lazylistindex"
          pushBool False
          rawset (nthBottom 4)
        else do
          -- put back remaining unevalled list
          void $ putuserdata @[itemtype] (nthBottom 1) lazyListStateName rest
          pushName "__lazylistindex"
          pushinteger (curindex + fromIntegral (length as))
          rawset (nthBottom 4)
      -- push evaluated elements
      forM_ (zip [(curindex + 1)..] as) $ \(i, a) -> do
        pushItem a
        rawseti (nthBottom 4) i
      return (NumResults 0)
    _ -> pure (NumResults 0)

-- | Name of the metatable used for unevaluated lazy list rema
lazyListStateName :: Name
lazyListStateName = "HsLua unevalled lazy list"

-- | Pushes a userdata value of the given type.
pushUD :: LuaError e => UDTypeWithList e fn a itemtype -> a -> LuaE e ()
pushUD ty x = do
  newhsuserdatauv x 1
  pushUDMetatable ty
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
peekUD :: LuaError e => UDTypeWithList e fn a itemtype -> Peeker e a
peekUD ty idx = do
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

-- | Gets a list from a uservalue table and sets it on the given value.
-- Expects the uservalue (i.e., caching) table to be at the top of the
-- stack.
setList :: forall itemtype e a. LuaError e
        => ListSpec e a itemtype -> a
        -> Peek e a
setList (_pushspec, (peekItem, updateList)) x = (x `updateList`) <$!> do
  liftLua (getfield top "__lazylistindex") >>= \case
    TypeBoolean -> do
      -- list had been fully evaluated
      liftLua $ pop 1
      peekList peekItem top
    _ -> do
      let getLazyList = do
            liftLua (getfield top "__lazylist") >>= \case
              TypeUserdata -> pure ()
              _ -> failPeek "unevaled items of lazy list cannot be peeked"
            (`lastly` pop 1) $ reportValueOnFailure
              lazyListStateName
              (\idx -> fromuserdata @[itemtype] idx lazyListStateName)
              top
      mlastIndex <- liftLua (tointeger top <* pop 1)
      let itemsAfter = case mlastIndex of
            Nothing -> const getLazyList
            Just lastIndex -> \i ->
              if i <= lastIndex
              then liftLua (rawgeti top i) >>= \case
                TypeNil -> [] <$ liftLua (pop 1)
                _ -> do
                  y <- peekItem top `lastly` pop 1
                  (y:) <$!> itemsAfter (i + 1)
              else getLazyList
      itemsAfter 1
