{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Documentation
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Provides a function to print documentation if available.
-}
module HsLua.Packaging.Documentation
  ( documentation
  , getdocumentation
  , registerDocumentation
  , pushModuleDoc
  , pushFunctionDoc
  , pushFieldDoc
  , docsField
  ) where

import Data.Version (showVersion)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.Packaging.Types

-- | Function that retrieves documentation.
documentation :: LuaError e => DocumentedFunction e
documentation =
  DocumentedFunction
  { callFunction = documentationHaskellFunction
  , functionName = "documentation"
  , functionDoc = FunctionDoc
    { functionDescription =
      "Retrieves the documentation of the given object."
    , parameterDocs =
      [ ParameterDoc
        { parameterName = "value"
        , parameterType = "any"
        , parameterDescription = "documented object"
        , parameterIsOptional = False
        }
      ]
    , functionResultsDocs =  ResultsDocList
      [ ResultValueDoc "string|nil" "docstring" ]
    , functionSince = Nothing
    }
  }

-- | Function that returns the documentation of a given object, or @nil@
-- if no documentation is available.
documentationHaskellFunction :: LuaError e => LuaE e NumResults
documentationHaskellFunction = isnoneornil (nthBottom 1) >>= \case
  True -> failLua "expected a non-nil value as argument 1"
  _ -> NumResults 1 <$ getdocumentation top

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

-- | Name of the registry field holding the documentation table. The
-- documentation table is indexed by the documented objects, like module
-- tables and functions, and contains documentation strings as values.
--
-- The table is an ephemeron table, i.e., an entry gets garbage
-- collected if the key is no longer reachable.
docsField :: Name
docsField = "HsLua docs"

-- | Pushes the documentation of a module as a table with string fields
-- @name@ and @description@.
pushModuleDoc :: LuaError e => Pusher e (Module e)
pushModuleDoc = pushAsTable
  [ ("name", pushName . moduleName)
  , ("description", pushText . moduleDescription)
  , ("fields", pushList pushFieldDoc . moduleFields)
  , ("functions", pushList pushFunctionDoc . moduleFunctions)
  ]

-- | Pushes the documentation of a field as a table with string fields
-- @name@ and @description@.
pushFieldDoc :: LuaError e => Pusher e (Field e)
pushFieldDoc = pushAsTable
  [ ("name", pushText . fieldName)
  , ("description", pushText . fieldDescription)
  ]

-- | Pushes the documentation of a function as a table with string
-- fields, @name@, @description@, and @since@, sequence field
-- @parameters@, and sequence or string field @results@.
pushFunctionDoc :: LuaError e => Pusher e (DocumentedFunction e)
pushFunctionDoc fun = pushAsTable
  [ ("name", pushName . const (functionName fun))
  , ("description", pushText . functionDescription)
  , ("parameters", pushList pushParameterDoc . parameterDocs)
  , ("results", pushResultsDoc . functionResultsDocs)
  , ("since", maybe pushnil (pushString . showVersion) . functionSince)
  ] (functionDoc fun)

-- | Pushes the documentation of a parameter as a table with boolean
-- field @optional@ and string fields @name@, @type@, and @description.
pushParameterDoc :: LuaError e => Pusher e ParameterDoc
pushParameterDoc = pushAsTable
  [ ("name", pushText . parameterName)
  , ("type", pushText . parameterType)
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
  [ ("type", pushText . resultValueType)
  , ("description", pushText . resultValueDescription)
  ]
