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
  , registerDocumentation
  ) where

import Data.Text (Text)
import HsLua.Core as Lua
import HsLua.Marshalling (pushText)
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
  _ -> do
    settop 1 -- allow just one argument
    -- retrieve documentation
    pushDocumentationTable
    pushvalue (nthBottom 1)
    _ <- rawget (nth 2)
    return (NumResults 1)

-- | Registers text as documentation for the object at the stack index
-- @idx@.
registerDocumentation :: LuaError e
                      => StackIndex  -- ^ @idx@
                      -> Text        -- ^ documentation string
                      -> LuaE e ()
registerDocumentation idx docs = do
  idx' <- absindex idx
  pushDocumentationTable
  pushvalue idx'  -- the documented object
  pushText docs   -- documentation string
  rawset (nth 3)  -- add to docs table
  pop 1           -- pop docs table

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

-- | Name of the registry field holding the documentation table.
docsField :: Name
docsField = "HsLua docs"
