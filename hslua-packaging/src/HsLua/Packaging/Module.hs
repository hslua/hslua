{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Module
Copyright   : © 2019-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions for HsLua modules.
-}
module HsLua.Packaging.Module
  ( -- * Documented module
    Module (..)
  , Field (..)
    -- * Constructors
    -- ** Module
  , defmodule
  , withFields
  , withFunctions
  , withOperations
  , associateType
  , renameTo
    -- ** Field
  , deffield
  , withType
  , withDescription
  , withValue
    -- ** Type Classes
  , HasName (..)
  , HasDescription (..)
    -- * Module Loading
  , registerModule
  , preloadModule
  , preloadModuleWithName
  , pushModule
  , Operation (..)
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import HsLua.Core
import HsLua.Marshalling (Pusher, pushAsTable, pushList, pushName, pushText)
import HsLua.ObjectOrientation.Operation (Operation (..), metamethodName)
import HsLua.Packaging.Documentation
import HsLua.Packaging.Types
import HsLua.Packaging.UDType (DocumentedType, initType)
import HsLua.Typing (TypeSpec, anyType)
import qualified HsLua.Core.Utf8 as Utf8
import qualified HsLua.Packaging.Function as Fun

-- | Define a Lua module.
defmodule :: Name -> Module e
defmodule name = Module
  { moduleName = name
  , moduleDescription = mempty
  , moduleFields = mempty
  , moduleFunctions = mempty
  , moduleOperations = mempty
  , moduleTypeInitializers = mempty
  }

-- | Set the list of module fields.
withFields :: Module e -> [Field e] -> Module e
withFields mdl fields = mdl { moduleFields = fields }

-- | Set the list of functions in the module.
withFunctions :: Module e -> [DocumentedFunction e] -> Module e
withFunctions mdl fns = mdl { moduleFunctions = fns }

-- | Set operations that can be performed on the module object.
withOperations :: Module e -> [(Operation, DocumentedFunction e)] -> Module e
withOperations mdl ops = mdl { moduleOperations = ops }

-- | Sets a textual description
withDescription :: HasDescription a => a -> Text -> a
withDescription = setDescription

-- | Associate a type with this module. An associated type is listed in the
-- module documentation.
associateType :: LuaError e => Module e -> DocumentedType e a -> Module e
associateType mdl tp =
  mdl { moduleTypeInitializers = initType tp : moduleTypeInitializers mdl }

-- | Gives a different name
renameTo :: HasName a => a -> Name -> a
renameTo = setName

infixl 0 `withFields`, `withFunctions`, `withDescription`, `withOperations`
infixl 0 `associateType`

--
-- Field constructor and setters
--

-- | Create a new module field.
deffield :: Name -> Field e
deffield name = Field
  { fieldName = name
  , fieldPushValue = return ()
  , fieldDoc = FieldDoc
      { fieldDocName = Utf8.toText $ fromName name
      , fieldDocType = anyType
      , fieldDocDescription = mempty
      }
  }

-- | Set a specific type for a field.
withType :: Field e -> TypeSpec -> Field e
withType fld typespec =
  let doc = fieldDoc fld
  in fld { fieldDoc = doc { fieldDocType = typespec }}

-- | Add a value pusher to a field.
withValue :: Field e -> LuaE e () -> Field e
withValue fld pusher = fld { fieldPushValue = pusher }

infixl 0 `withType`, `withValue`

-- | Create a new module (i.e., a Lua table).
create :: LuaE e ()
create = newtable

-- | Registers a 'Module'; leaves a copy of the module table on
-- the stack.
registerModule :: LuaError e => Module e -> LuaE e ()
registerModule mdl =
  requirehs (moduleName mdl) (const (pushModule mdl))

-- | Add the module under a different name to the table of preloaded
-- packages.
preloadModuleWithName :: LuaError e => Module e -> Name -> LuaE e ()
preloadModuleWithName documentedModule name = preloadModule $
  documentedModule { moduleName = name }

-- | Preload self-documenting module using the module's default name.
preloadModule :: LuaError e => Module e -> LuaE e ()
preloadModule mdl =
  preloadhs (moduleName mdl) $ do
    pushModule mdl
    return (NumResults 1)

-- | Pushes a documented module to the Lua stack.
pushModule :: LuaError e => Module e -> LuaE e ()
pushModule mdl = do
  checkstack' 10 "pushModule"
  pushAsTable
    [ ("name", pushName . moduleName)
    , ("description", pushText . moduleDescription)
    , ("fields", pushList (pushFieldDoc . fieldDoc) . moduleFields)
    , ("types", pushTypesFunction . moduleTypeInitializers)
    ] mdl
  create        -- module table
  pushvalue (nth 2)              -- push documentation object
  registerDocumentation (nth 2)  -- set and pop doc

  -- # Functions
  --
  -- module table now on top
  -- documentation table in pos 2
  newtable -- function documention
  pushName "functions"
  pushvalue (nth 2)
  rawset (nth 5)
  -- function documentation table now on top
  -- module table in position 2
  -- module documentation table in pos 3
  forM_ (zip [1..] (moduleFunctions mdl)) $ \(i, fn) -> do
    -- push documented function, thereby registering the function docs
    Fun.pushDocumentedFunction fn
    -- add function to module
    pushName (functionName fn)
    pushvalue (nth 2) -- C function
    rawset (nth 5)    -- module table
    -- set documentation
    _ <- getdocumentation top
    rawseti (nth 3) i
    pop 1 -- C Function
  pop 1 -- function documentation table
  remove (nth 2) -- module documentation table

  -- # Fields
  --
  forM_ (moduleFields mdl) $ \fld -> do
    pushName (fieldName fld)
    fieldPushValue fld
    rawset (nth 3)
  case moduleOperations mdl of
    [] -> pure ()
    ops -> do
      -- create a metatable for this module and add operations
      newtable
      forM_ ops $ \(op, fn) -> do
        pushName $ metamethodName op
        Fun.pushDocumentedFunction $ fn `setName` ""
        rawset (nth 3)
      setmetatable (nth 2)

pushTypesFunction :: LuaError e => Pusher e [LuaE e Name]
pushTypesFunction initializers = pushHaskellFunction $ do
  sequence initializers >>= pushList pushName
  pure 1
