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

import Data.Version (showVersion)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation (UDTypeGeneric (..))
import HsLua.Packaging.Types
import HsLua.Typing (pushTypeSpec)
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
pushDocumentationObject :: LuaError e => Pusher e DocumentationObject
pushDocumentationObject obj = do
  newhsuserdatauv obj 0
  pushDocumentationObjectMT
  setmetatable (nth 2)

-- | Pushes the metatable for documentation objects.
pushDocumentationObjectMT :: LuaError e => LuaE e ()
pushDocumentationObjectMT = newudmetatable documentationObjectName >>= \case
  False -> return ()
  True -> do -- newly created metatable at the top of the stack
    -- Allow to "call" the documentation object, in which case it should
    -- return a Lua table that has all the relevant info.
    pushHaskellFunction $ do
      -- object is the first argument
      forcePeek (peekDocumentationObject (nthBottom 1)) >>= \case
        DocObjectFunction fn -> pushFunctionDocAsTable fn
        DocObjectModule mdl  -> pushModuleDocAsTable mdl
        DocObjectType ty     -> pushTypeDocAsTable ty
      return (NumResults 1)
    setfield (nth 2) "__call"

-- | Pushes the documentation of a module as userdata.
pushModuleDoc :: LuaError e => Pusher e ModuleDoc
pushModuleDoc = pushDocumentationObject . DocObjectModule

-- | Retrieves a module documentation object from the Lua stack.
peekModuleDoc :: Peeker e ModuleDoc
peekModuleDoc idx = peekDocumentationObject idx >>= \case
  DocObjectModule mdldoc -> pure mdldoc
  _ -> failPeek "Not a module documentation object"

-- | Pushes function documentation as userdata.
pushFunctionDoc :: LuaError e => Pusher e FunctionDoc
pushFunctionDoc = pushDocumentationObject . DocObjectFunction

-- | Retrieve function documentation from the Lua stack.
peekFunctionDoc :: Peeker e FunctionDoc
peekFunctionDoc idx = peekDocumentationObject idx >>= \case
  DocObjectFunction fndoc -> pure fndoc
  _ -> failPeek "Not a function documentation"

-- | Pushes documentation type documentation as userdata.
pushTypeDoc :: LuaError e => Pusher e FunctionDoc
pushTypeDoc = pushDocumentationObject . DocObjectFunction

-- | Retrieve function documentation from the Lua stack.
peekTypeDoc :: Peeker e TypeDoc
peekTypeDoc idx = peekDocumentationObject idx >>= \case
  DocObjectType tydoc -> pure tydoc
  _ -> failPeek "Not a type documentation"


-- | Pushes the documentation of a module as a table with string fields
-- @name@ and @description@.
pushModuleDocAsTable :: LuaError e => Pusher e ModuleDoc
pushModuleDocAsTable = pushAsTable
  [ ("name", pushText . moduleDocName)
  , ("description", pushText . moduleDocDescription)
  , ("fields", pushList pushFieldDocAsTable . moduleDocFields)
  , ("functions", pushList pushFunctionDocAsTable . moduleDocFunctions)
  , ("types", pushList pushTypeDocAsTable . moduleDocTypes)
  ]

-- | Pushes the documentation of a field as a table with string fields
-- @name@ and @description@.
pushFieldDocAsTable :: LuaError e => Pusher e FieldDoc
pushFieldDocAsTable = pushAsTable
  [ ("name", pushText . fieldDocName)
  , ("type", pushTypeSpec . fieldDocType)
  , ("description", pushText . fieldDocDescription)
  ]

-- | Pushes the documentation of a function as a table with string
-- fields, @name@, @description@, and @since@, sequence field
-- @parameters@, and sequence or string field @results@.
pushFunctionDocAsTable :: LuaError e => Pusher e FunctionDoc
pushFunctionDocAsTable = pushAsTable
  [ ("name", pushText . funDocName)
  , ("description", pushText . funDocDescription)
  , ("parameters", pushList pushParameterDocAsTable . funDocParameters)
  , ("results", pushResultsDoc . funDocResults)
  , ("since", maybe pushnil (pushString . showVersion) . funDocSince)
  ]

-- | Pushes the documentation of a parameter as a table with boolean
-- field @optional@ and string fields @name@, @type@, and @description@.
pushParameterDocAsTable :: LuaError e => Pusher e ParameterDoc
pushParameterDocAsTable = pushAsTable
  [ ("name", pushText . parameterName)
  , ("type", pushTypeSpec . parameterType)
  , ("description", pushText . parameterDescription)
  , ("optional", pushBool . parameterIsOptional)
  ]

-- | Pushes a the documentation for a function's return values as either
-- a simple string, or as a sequence of tables with @type@ and
-- @description@ fields.
pushResultsDoc :: LuaError e => Pusher e ResultsDoc
pushResultsDoc = \case
  ResultsDocMult desc -> pushText desc
  ResultsDocList resultDocs -> pushList pushResultValueDoc resultDocs

-- | Pushes the documentation of a single result value as a table with
-- fields @type@ and @description@.
pushResultValueDoc :: LuaError e => Pusher e ResultValueDoc
pushResultValueDoc = pushAsTable
  [ ("type", pushTypeSpec . resultValueType)
  , ("description", pushText . resultValueDescription)
  ]

-- | Pushes the documentation of a UDType as a Lua table.
pushTypeDocAsTable :: LuaError e => Pusher e TypeDoc
pushTypeDocAsTable = pushAsTable
  [ ("name", pushText . typeDocName)
  , ("description", pushText . typeDocDescription)
  , ("methods", pushList pushFunctionDoc . typeDocMethods)
  ]
