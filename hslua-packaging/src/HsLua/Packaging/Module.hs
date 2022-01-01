{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Module
Copyright   : © 2019-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions for HsLua modules.
-}
module HsLua.Packaging.Module
  ( -- * Documented module
    Module (..)
  , Field (..)
  , registerModule
  , preloadModule
  , preloadModuleWithName
  , pushModule
  , Operation (..)
  )
where

import Control.Monad (forM_)
import HsLua.Core
import HsLua.Marshalling (pushName, pushText)
import HsLua.ObjectOrientation.Operation (Operation (..), metamethodName)
import HsLua.Packaging.Types
import qualified HsLua.Packaging.Function as Call

-- | Create a new module (i.e., a Lua table).
create :: LuaE e ()
create = newtable

-- | Registers a 'Module'; leaves a copy of the module table on
-- the stack.
registerModule :: LuaError e => Module e -> LuaE e ()
registerModule mdl =
  requirehs (moduleName mdl) (pushModule mdl)

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
  create
  forM_ (moduleFunctions mdl) $ \fn -> do
    pushName (functionName fn)
    Call.pushDocumentedFunction fn
    rawset (nth 3)
  forM_ (moduleFields mdl) $ \field -> do
    pushText (fieldName field)
    fieldPushValue field
    rawset (nth 3)
  case moduleOperations mdl of
    [] -> pure ()
    ops -> do
      -- create a metatable for this module and add operations
      newtable
      forM_ ops $ \(op, fn) -> do
        pushName $ metamethodName op
        Call.pushDocumentedFunction $ Call.setName "" fn
        rawset (nth 3)
      setmetatable (nth 2)
