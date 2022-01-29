{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Rendering
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : Portable

Render function and module documentation.
-}
module HsLua.Packaging.Rendering
  {-# DEPRECATED "Use getdocumentation with a custom renderer." #-}
  ( -- * Documentation
    render
  , renderModule
  , renderFunction
  ) where

import Data.Text (Text)
import Data.Version (showVersion)
import HsLua.Core
import HsLua.Packaging.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified HsLua.Core.Utf8 as Utf8

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

--
-- Module documentation
--

-- | Alias for 'renderModule'.
render :: Module e -> Text
render = renderModule

-- | Renders module documentation as Markdown.
renderModule :: Module e -> Text
renderModule mdl =
  let fields = moduleFields mdl
  in T.unlines
     [ "# " <> T.decodeUtf8 (fromName $ moduleName mdl)
     , ""
     , moduleDescription mdl
     , renderFields fields
     , renderFunctions (moduleFunctions mdl)
     ]

-- | Renders the full function documentation section.
renderFunctions :: [DocumentedFunction e] -> Text
renderFunctions = \case
  [] -> mempty
  fs -> "\n## Functions\n\n"
        <> T.intercalate "\n\n" (map (("### " <>) . renderFunction) fs)

-- | Renders documentation of a function.
renderFunction :: DocumentedFunction e  -- ^ function
               -> Text                  -- ^ function docs
renderFunction fn =
  let fnDoc = functionDoc fn
      fnName = Utf8.toText $ fromName (functionName fn)
      name = if T.null fnName
             then "<anonymous function>"
             else fnName
  in T.intercalate "\n"
     [ name <> " (" <> renderFunctionParams fnDoc <> ")"
     , ""
     , renderFunctionDoc fnDoc
     ]

-- | Renders the parameter names of a function, separated by commas.
renderFunctionParams :: FunctionDoc -> Text
renderFunctionParams fd =
    T.intercalate ", "
  . map parameterName
  $ parameterDocs fd

-- | Render documentation for fields as Markdown.
renderFields :: [Field e] -> Text
renderFields fs =
  if null fs
  then mempty
  else mconcat
       [ "\n"
       , T.intercalate "\n\n" (map (("### " <>) . renderField) fs)
       ]

-- | Renders documentation for a single field.
renderField :: Field e -> Text
renderField f = fieldName f <> "\n\n" <> fieldDescription f

--
-- Function documentation
--

-- | Renders the documentation of a function as Markdown.
renderFunctionDoc :: FunctionDoc -> Text
renderFunctionDoc (FunctionDoc desc paramDocs resultDoc mVersion) =
  let sinceTag = case mVersion of
        Nothing -> mempty
        Just version -> T.pack $ "\n\n*Since: " <> showVersion version <> "*"
  in (if T.null desc
      then ""
      else desc <> sinceTag <> "\n\n") <>
     renderParamDocs paramDocs <>
     renderResultsDoc resultDoc

-- | Renders function parameter documentation as a Markdown blocks.
renderParamDocs :: [ParameterDoc] -> Text
renderParamDocs pds = "Parameters:\n\n" <>
  T.intercalate "\n" (map renderParamDoc pds)

-- | Renders the documentation of a function parameter as a Markdown
-- line.
renderParamDoc :: ParameterDoc -> Text
renderParamDoc pd = mconcat
  [ parameterName pd
  ,  "\n:   "
  , parameterDescription pd
  , " (", parameterType pd, ")\n"
  ]

-- | Renders the documentation of a function result as a Markdown list
-- item.
renderResultsDoc :: ResultsDoc -> Text
renderResultsDoc = \case
  ResultsDocList []  -> mempty
  ResultsDocList rds ->
    "\nReturns:\n\n" <> T.intercalate "\n" (map renderResultValueDoc rds)
  ResultsDocMult txt -> " -  " <> indent 4 txt

-- | Renders the documentation of a function result as a Markdown list
-- item.
renderResultValueDoc :: ResultValueDoc -> Text
renderResultValueDoc rd = mconcat
  [ " -  "
  , resultValueDescription rd
  , " (", resultValueType rd, ")"
  ]

indent :: Int -> Text -> Text
indent n = T.replace "\n" (T.replicate n " ")
