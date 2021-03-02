{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Module
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions for HsLua modules.
-}
module HsLua.Module
  ( requirehs
  , preloadhs
  , addfield
  , addfunction
  , create
    -- * Module
  , Module (..)
  , Field (..)
  , registerModule
  , preloadModule
  , pushModule
    -- * Documentation
  , render
  )
where

import Control.Monad (forM_, void)
import Data.Text (Text)
import HsLua.Call (HaskellFunction)
import HsLua.Core
import HsLua.Push (pushText)
import HsLua.Types.Pushable (Pushable, push)
import HsLua.Types.Exposable
  ( Exposable
  , pushHaskellFunction
  )
import qualified Data.Text as T
import qualified HsLua.Call as Call

-- | Load a module, defined by a Haskell action, under the given
-- name.
--
-- Similar to @luaL_required@: After checking "loaded" table,
-- calls @pushMod@ to push a module to the stack, and registers
-- the result in @package.loaded@ table.
--
-- The @pushMod@ function must push exactly one element to the top
-- of the stack. This is not checked, but failure to do so will
-- lead to problems. Lua's @package@ module must have been loaded
-- by the time this function is invoked.
--
-- Leaves a copy of the module on the stack.
requirehs :: String -> Lua () -> Lua ()
requirehs modname pushMod = do
  -- get table of loaded modules
  void $ getfield registryindex loadedTableRegistryField

  -- Check whether module has already been loaded.
  getfield top modname >>= \case -- LOADED[modname]
    TypeNil -> do   -- not loaded yet, load now
      pop 1  -- remove field
      pushMod  -- push module
      pushvalue top  -- make copy of module
      -- add module under the given name (LOADED[modname] = module)
      setfield (nth 3) modname
    _ -> return ()

  remove (nth 2)  -- remove table of loaded modules

-- | Registers a preloading function. Takes an module name and the
-- Lua operation which produces the package.
preloadhs :: String -> Lua NumResults -> Lua ()
preloadhs name pushMod = do
  void $ getfield registryindex preloadTableRegistryField
  pushHaskellFunction pushMod
  setfield (nth 2) name
  pop 1

-- | Add a string-indexed field to the table at the top of the
-- stack.
addfield :: Pushable a => String -> a -> Lua ()
addfield name value = do
  push name
  push value
  rawset (nth 3)

-- | Attach a function to the table at the top of the stack, using
-- the given name.
addfunction :: Exposable a => String -> a -> Lua ()
addfunction name fn = do
  push name
  pushHaskellFunction fn
  rawset (nth 3)

-- | Create a new module (i.e., a Lua table).
create :: Lua ()
create = newtable

-- | Named and documented Lua module.
data Module = Module
  { moduleName :: Text
  , moduleDescription :: Text
  , moduleFields :: [Field]
  , moduleFunctions :: [(Text, HaskellFunction)]
  }

-- | Self-documenting module field
data Field = Field
  { fieldName :: Text
  , fieldDescription :: Text
  , fieldPushValue :: Lua ()
  }


-- | Registers a 'Module'; leaves a copy of the module table on
-- the stack.
registerModule :: Module -> Lua ()
registerModule mdl =
  requirehs (T.unpack $ moduleName mdl) (pushModule mdl)

-- | Preload self-documenting module.
preloadModule :: Module -> Lua ()
preloadModule mdl =
  preloadhs (T.unpack $ moduleName mdl) $ do
    pushModule mdl
    return (NumResults 1)

pushModule :: Module -> Lua ()
pushModule mdl = do
  create
  forM_ (moduleFunctions mdl) $ \(name, fn) -> do
    pushText name
    Call.pushHaskellFunction fn
    rawset (nth 3)

-- | Renders module documentation as Markdown.
render :: Module -> Text
render mdl =
  let fields = moduleFields mdl
  in T.unlines
     [ "# " <> moduleName mdl
     , ""
     , moduleDescription mdl
     , if null (moduleFields mdl) then "" else renderFields fields
     , "## Functions"
     , ""
     ]
     <> T.intercalate "\n"
        (map (uncurry renderFunctionDoc) (moduleFunctions mdl))

-- | Renders documentation of a function.
renderFunctionDoc :: Text             -- ^ name
                  -> HaskellFunction  -- ^ function
                  -> Text             -- ^ function docs
renderFunctionDoc name fn =
  case Call.functionDoc fn of
    Nothing -> ""
    Just fnDoc -> T.intercalate "\n"
      [ "### " <> name <> " (" <> renderFunctionParams fnDoc <> ")"
      , ""
      , Call.render fnDoc
      ]

renderFunctionParams :: Call.FunctionDoc -> Text
renderFunctionParams fd =
    T.intercalate ", "
  . map Call.parameterName
  $ Call.parameterDocs fd

-- | Render documentation for fields as Markdown.
renderFields :: [Field] -> Text
renderFields fs =
  if null fs
  then mempty
  else T.unlines $ map renderField fs

-- | Renders documentation for a single field.
renderField :: Field -> Text
renderField f =
  "### " <> fieldName f <> "\n\n" <> fieldDescription f <> "\n"
