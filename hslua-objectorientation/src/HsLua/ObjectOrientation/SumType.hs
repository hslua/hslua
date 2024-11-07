{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-|
Module      : HsLua.ObjectOrientation.SumType
Copyright   : © 2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Types and functions to use Abstract Data Types in Lua, and to change the
object behavior depending on the underlying constructor.
-}
module HsLua.ObjectOrientation.SumType
  ( UDSumTypeGeneric
  , OOSumType (..)
  , Constructor (..)
    -- * Defining types
  , defsumtypeGeneric
  , defconstructor
  , constructorProperty
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Data
import Data.Map (Map)
import Data.String (fromString)
import Data.Text (Text)
import Foreign.C (CInt(..))
import Foreign.Ptr (FunPtr)
import HsLua.Core as Lua
import qualified HsLua.Core.Types as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation.Generic
import HsLua.ObjectOrientation.Operation
import HsLua.Typing ( TypeSpec (..) )
import qualified Data.Map.Strict as Map
import qualified HsLua.Core.Utf8 as Utf8

-- | A sum type.
type UDSumTypeGeneric e fn a =
  UDTypeGeneric e fn a (OOSumType e a)

-- | Object extension to add constructor-specific behavior to objects.
data OOSumType e a = OOSumType
  { ooSumConstructors :: !(Map Name (Constructor e a))
  , ooSumTag          :: !(a -> Name)
  }

-- | Constructor for a summand in a sum type.
data Constructor e a = Constr
  { constrName        :: !Name
  , constrDescription :: !Text
  , constrProperties  :: !(Map Name (Property e a))
  }

defconstructor :: Name -> Text -> [(Name, Property e a)] -> Constructor e a
defconstructor name descr prop =
  Constr name descr (Map.fromList prop)

constructorProperty
  :: LuaError e
  => Name                               -- ^ property name
  -> TypeSpec                           -- ^ type of the property value
  -> Text                               -- ^ property description
  -> (Pusher e b, a -> Possible b)      -- ^ how to get the property value
  -> (Peeker e b, a -> b -> Possible a) -- ^ how to set a new property value
  -> (Name, Property e a)
constructorProperty name typespec desc (push, get) (peek, set) =
  ( name
  , Property
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
  )

-- | Defines a new type that could also be treated as a list; defines
-- the behavior of objects in Lua. Note that the type name must be
-- unique.
defsumtypeGeneric
  :: Pusher e fn          -- ^ function pusher
  -> Name                 -- ^ type name
  -> [(Operation, fn)]    -- ^ operations
  -> [Member e fn a]      -- ^ methods
  -> (a -> Name)
  -> [Constructor e a]
  -> UDSumTypeGeneric e fn a
defsumtypeGeneric fn name ops members tagName constrs =
  deftypeGeneric' fn name ops members $
    OOSumType
    { ooSumConstructors =
        Map.fromList $ map (\c -> (constrName c, c)) constrs
    , ooSumTag = tagName
    }

tagMap :: DataType -> Map.Map ConIndex String
tagMap = Map.fromList . map (constrIndex &&& showConstr) . dataTypeConstrs

tagIdxAssoc :: Data a => a -> [(Name, ConIndex)]
tagIdxAssoc = map (\(a, b) -> (fromString b, a)) . Map.toList . tagMap . dataTypeOf

instance (Data a, LuaError e) => UDTypeExtension e a (OOSumType e a) where
  extensionMetatableSetup ty = do
    addOp Index    $ pushcfunction hslua_sum_udindex_ptr
    addOp Newindex $ pushcfunction hslua_sum_udnewindex_ptr
    addField "tags" $
      pushMap pushIntegral pushString (tagMap $ dataTypeOf (undefined :: a))
    _ <- getfield top "getters"
    addField "tag" $ pushcfunction hslua_sum_get_tag_ptr
    pop 1 -- getters table
    forM_ (ooSumConstructors $ udExtension ty) $ \constr -> do
      addField (constrName constr) $ do
        newtable
        addField "getters" $ do
          let fn p = forcePeek (peekUDGeneric ty 1) >>= propertyGet p
          pushMap pushName (pushHaskellFunction . fn) (constrProperties constr)
        addField "setters" $ do
          let pushSetter = const $ pushcfunction hslua_udsetter_ptr
          pushMap pushName pushSetter (constrProperties constr)
        pushvalue top
        case lookup (constrName constr) (tagIdxAssoc (undefined :: a)) of
          Just i -> rawseti (nth 4) (fromIntegral i)
          Nothing -> failLua "Unknown sum-type constructor"

  extensionPeekUD ty x idx = do
    tag <- liftLua $ do
      l <- state
      _ <- Lua.liftIO $ hslua_gettag l idx
      forcePeek $ peekName top `lastly` pop 1
    let constrs = ooSumConstructors $ udExtension ty
    case Map.lookup tag constrs of
      Nothing -> return x
      Just constr -> do
        let props = constrProperties constr
        liftLua $ getiuservalue idx 1 >>= \case
          TypeTable -> do
            pushnil
            setProperties props x <* pop 1
          _otherwise -> x <$ pop 1

  extensionPushUD _ty x = do
    -- let tag = (ooSumTag $ udExtension ty) x
    -- pushName tag
    let tagIdx = constrIndex $ toConstr x
    pushIntegral tagIdx
    setiuservalue (nth 2) 2 >>= \case
      True -> pure ()
      False -> failLua "Couldn't set tag, object has no second uservalue."

  extensionUservalues _ty = 2

-- | Sets field @name@ in the table at the top of the stack to the value
-- pushed by @pushValue@. The @pushValue@ action must push exactly one
-- value to the Lua stack.
addField :: LuaError e
         => Name        -- ^ name
         -> LuaE e ()   -- ^ pushValue
         -> LuaE e ()
addField name pushValue = do
  pushName name
  pushValue
  rawset (nth 3)

-- | Like 'addField', but adds the table function that governs
-- @operation@. The @pushFn@ action must push exactly one value to the
-- Lua stack.
addOp :: LuaError e
      => Operation      -- ^ operation
      -> LuaE e ()      -- ^ pushFn
      -> LuaE e ()
addOp op = addField (metamethodName op)

-- | Retrieves a key from a Haskell-data holding userdata value.
--
-- Does the following, in order, and returns the first non-nil result:
--
--   - Checks the userdata's caching table for the given key;
--
--   - Checks whether the constructor corresponding to the wrapped
--     element has a getter for this value and uses that.
--
--   - Falls back to the default @__index@ lookup for objects.
foreign import ccall "hslsum.c &hslua_sum_udindex"
  hslua_sum_udindex_ptr :: FunPtr (State -> IO NumResults)

-- | Sets a new value in the userdata caching table via a setter
-- functions.
--
-- Delegates the caching assignment to a function either in the
-- constructor-specific @setters@ table or the global @setters@ table.
foreign import ccall "hslsum.c &hslua_sum_udnewindex"
  hslua_sum_udnewindex_ptr :: FunPtr (State -> IO NumResults)

-- | Get the sum-type's @tag@, i.e., the constructor name.
foreign import ccall "hslsum.c &hslua_sum_get_tag"
  hslua_sum_get_tag_ptr :: FunPtr (State -> IO NumResults)

-- | Get the sum-type's @tag@, i.e., the constructor name.
foreign import ccall "hslsum.c hslua_gettag"
  hslua_gettag :: (State -> StackIndex -> IO CInt)

-- | Sets a value in the caching table.
foreign import ccall "hslobj.c &hslua_udsetter"
  hslua_udsetter_ptr :: FunPtr (State -> IO NumResults)
