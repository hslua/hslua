{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Documentation
Copyright   : © 2020-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Provides a function to print documentation if available.
-}
module HsLua.Packaging.Documentation
  ( -- * Setting and retrieving documentation
    getdocumentation
  , registerDocumentation
  , docsField
    -- * Documentation Types
  , ModuleDoc (..)
  , FunctionDoc (..)
  , DocumentationObject (..)
  , pushDocumentationObject
  , peekDocumentationObject
  , pushModuleDoc
  , peekModuleDoc
  , pushFunctionDoc
  , peekFunctionDoc
  , pushTypeDoc
  , peekTypeDoc
  -- * Creating documentation values
  , generateFunctionDocumentation
  , generateModuleDocumentation
  , generateTypeDocumentation
  ) where

import Control.Monad (void)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation (UDTypeGeneric (..))
import HsLua.Packaging.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as Utf8

-- | Pushes the documentation for the element at the given stack index.
-- Returns the type of the documentation object.
getdocumentation :: LuaError e => StackIndex -> LuaE e Lua.Type
getdocumentation idx = do
  idx' <- absindex idx
  pushDocumentationTable
  pushvalue idx'
  rawget (nth 2) <* Lua.remove (nth 2)  -- remove documentation table

-- | Registers the object at the top of the stack as documentation for
-- the object at index @idx@. Pops the documentation of the stack.
registerDocumentation :: LuaError e
                      => StackIndex  -- ^ @idx@
                      -> LuaE e ()
registerDocumentation idx = do
  checkstack' 10 "registerDocumentation"  -- keep some buffer
  idx' <- absindex idx
  pushDocumentationTable
  pushvalue idx'    -- the documented object
  pushvalue (nth 3) -- documentation object
  rawset (nth 3)    -- add to docs table
  pop 2             -- docs table and documentation object

-- | Name of the registry field holding the documentation table. The
-- documentation table is indexed by the documented objects, like module
-- tables and functions, and contains documentation objects as values.
--
-- The table is an ephemeron table, i.e., an entry gets garbage
-- collected if the key is no longer reachable.
docsField :: Name
docsField = "HsLua docs"

-- | Pushes the documentation table that's stored in the registry to the
-- top of the stack, creating it if necessary. The documentation table
-- is indexed by the documented objects, like module tables and
-- functions, and contains documentation strings as values.
--
-- The table is an ephemeron table, i.e., an entry gets garbage
-- collected if the key is no longer reachable.
pushDocumentationTable :: LuaError e => LuaE e ()
pushDocumentationTable = Lua.getfield registryindex docsField >>= \case
  Lua.TypeTable -> return () -- documentation table already initialized
  _ -> do
    pop 1            -- pop non-table value
    newtable         -- create documentation table
    pushstring "k"   -- Make it an "ephemeron table" and..
    setfield (nth 2) "__mode"  -- collect docs if documented object is GCed
    pushvalue top    -- add copy of table to registry
    setfield registryindex docsField

--
-- Generating
--

-- | Generate documentation for a module.
generateModuleDocumentation :: Module e -> ModuleDoc
generateModuleDocumentation mdl =
  let name = moduleName mdl
  in ModuleDoc
    { moduleDocName = nameToText name
    , moduleDocDescription = moduleDescription mdl
    , moduleDocFields = map (generateFieldDocumentation name) $ moduleFields mdl
    , moduleDocFunctions = map (generateFunctionDocumentation Nothing) $
                               moduleFunctions mdl
    , moduleDocTypes = moduleTypeDocs mdl
    }

-- | Generate 'FieldDoc' documentation for a module field.
generateFieldDocumentation :: Name     -- ^ module name
                           -> Field e  -- ^ field that's part of the module
                           -> FieldDoc
generateFieldDocumentation mdlName fld =
  let doc = fieldDoc fld
  in doc { fieldDocName = nameToText mdlName <> "." <> fieldDocName doc }

-- | Generate 'FunctionDoc' documentation for module functions.
generateFunctionDocumentation :: Maybe Name
                              -> DocumentedFunction e
                              -> FunctionDoc
generateFunctionDocumentation name fn =
  let doc = functionDoc fn
      prefix = maybe mempty (\n -> nameToText n <> ".") name
  in doc { funDocName = prefix <> funDocName doc }

-- | Generate documentation for a 'UDType'.
generateTypeDocumentation :: DocumentedType e a -> TypeDoc
generateTypeDocumentation ty =
  let name = udName ty
  in TypeDoc
  { typeDocName = nameToText name
  , typeDocDescription = ""
  , typeDocOperations = []
  , typeDocMethods = map (generateFunctionDocumentation (Just name) . snd) $
                         Map.toList (udMethods ty)
  }

-- | Convert a Lua name to UTF-8 text.
nameToText :: Name -> T.Text
nameToText = Utf8.toText . fromName

--
-- Retrieving and pushing documentation
--

-- | The metatable name of documentation objecs
documentationObjectName :: Name
documentationObjectName = "HsLua DocumentationObject"

-- | Pushes the metatable for documentation objects.
peekDocumentationObject :: Peeker e DocumentationObject
peekDocumentationObject idx = do
  liftLua (fromuserdata idx documentationObjectName) >>= \case
    Nothing  -> failPeek "Not a documentation object"
    Just doc -> pure doc

-- | Pushes a 'DocumentationObject' to the Lua stack.
pushDocumentationObject :: Pusher e DocumentationObject
pushDocumentationObject obj = do
  newhsuserdatauv obj 0
  pushDocumentationObjectMT
  setmetatable (nth 2)

-- | Pushes the metatable for documentation objects.
pushDocumentationObjectMT :: LuaE e ()
pushDocumentationObjectMT = void $ newudmetatable documentationObjectName

-- | Pushes the documentation of a module as userdata.
pushModuleDoc :: Pusher e ModuleDoc
pushModuleDoc = pushDocumentationObject . DocObjectModule

-- | Retrieves a module documentation object from the Lua stack.
peekModuleDoc :: Peeker e ModuleDoc
peekModuleDoc idx = peekDocumentationObject idx >>= \case
  DocObjectModule mdldoc -> pure mdldoc
  _ -> failPeek "Not a module documentation object"

-- | Pushes function documentation as userdata.
pushFunctionDoc :: Pusher e FunctionDoc
pushFunctionDoc = pushDocumentationObject . DocObjectFunction

-- | Retrieve function documentation from the Lua stack.
peekFunctionDoc :: Peeker e FunctionDoc
peekFunctionDoc idx = peekDocumentationObject idx >>= \case
  DocObjectFunction fndoc -> pure fndoc
  _ -> failPeek "Not a function documentation"

-- | Pushes documentation type documentation as userdata.
pushTypeDoc :: Pusher e FunctionDoc
pushTypeDoc = pushDocumentationObject . DocObjectFunction

-- | Retrieve function documentation from the Lua stack.
peekTypeDoc :: Peeker e TypeDoc
peekTypeDoc idx = peekDocumentationObject idx >>= \case
  DocObjectType tydoc -> pure tydoc
  _ -> failPeek "Not a type documentation"
