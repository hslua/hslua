{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Foreign.Lua.Module.Text
Copyright   : © 2017–2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : ForeignFunctionInterface

Provide a lua module containing a selection of useful Text functions.
-}
module Foreign.Lua.Module.Text

  ( -- * Module
    pushModule
  , preloadModule
  , documentedModule
  , description
  , functions

    -- * Legacy
  , pushModuleText
  , preloadTextModule
  ) where

import Prelude hiding (reverse)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Foreign.Lua (NumResults, Lua, Peekable, Pushable, ToHaskellFunction)
import Foreign.Lua.Call
import Foreign.Lua.Module hiding (preloadModule, pushModule)
import Foreign.Lua.Peek (Peeker, peekIntegral, peekText)
import Foreign.Lua.Push (pushIntegral, pushText)
import qualified Foreign.Lua as Lua
import qualified Data.Text as T

import qualified Foreign.Lua.Module as Module
--
-- Module
--

-- | Textual description of the "text" module.
description :: Text
description =
  "UTF-8 aware text manipulation functions, implemented in Haskell."

documentedModule :: Module
documentedModule = Module
  { moduleName = "paths"
  , moduleFields = []
  , moduleDescription = description
  , moduleFunctions = functions
  }

-- | Pushes the @text@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = 1 <$ Module.pushModule documentedModule

pushModuleText :: Lua NumResults
pushModuleText = 1 <$ Module.pushModule documentedModule

-- | Add the @text@ module under the given name to the table of
-- preloaded packages.
preloadModule :: String -> Lua ()
preloadModule name = Module.preloadModule $
  documentedModule { moduleName = T.pack name }

-- | Add the text module under the given name to the table of preloaded
-- packages.
preloadTextModule :: String -> Lua ()
preloadTextModule = flip Lua.preloadhs pushModuleText

--
-- Functions
--

functions :: [(Text, HaskellFunction)]
functions =
  [ ("len", len)
  , ("lower", lower)
  , ("reverse", reverse)
  , ("sub", sub)
  , ("upper", upper)
  ]

-- | Wrapper for @'T.length'@.
len :: HaskellFunction
len = toHsFnPrecursor T.length
  <#> textParam "s"
  =#> intResult "length"
  #? "Determines the number of characters in a string."

-- | Wrapper for @'T.toLower'@.
lower :: HaskellFunction
lower = toHsFnPrecursor T.toLower
  <#> textParam "s"
  =#> textResult "Lowercase copy of `s`"
  #? "Convert a string to lower case"

-- | Wrapper for @'T.reverse'@.
reverse :: HaskellFunction
reverse = toHsFnPrecursor T.reverse
  <#> textParam "s"
  =#> textResult "Reversed `s`"
  #? "Reverses a string."

-- | Returns a substring, using Lua's string indexing rules.
sub :: HaskellFunction
sub = toHsFnPrecursor substring
  <#> textParam "s"
  <#> textIndex "i" "substring start position"
  <#> textOptionalIndex "j" "substring end position"
  =#> textResult "text substring"
  #? "Returns a substring, using Lua's string indexing rules."
  where
    substring :: Text -> Int -> Maybe Int -> Text
    substring s i jopt =
      let j = fromMaybe (-1) jopt
          fromStart = if i >= 0 then  i - 1 else T.length s + i
          fromEnd   = if j <  0 then -j - 1 else T.length s - j
      in T.dropEnd fromEnd . T.drop fromStart $ s

-- | Wrapper for @'T.toUpper'@.
upper :: HaskellFunction
upper = toHsFnPrecursor T.toUpper
  <#> textParam "s"
  =#> textResult "Lowercase copy of `s`"
  #? "Convert a string to lower case"

--
-- Parameters
--

textParam :: Text -> Parameter Text
textParam name =
  parameter peekText "string" name "UTF-8 encoded string"

textIndex :: Text -> Text -> Parameter Int
textIndex = parameter (peekIntegral @Int) "integer"

textOptionalIndex :: Text -> Text -> Parameter (Maybe Int)
textOptionalIndex = optionalParameter (peekIntegral @Int) "integer"

--
-- Results
--

textResult :: Text -- ^ Description
           -> FunctionResults Text
textResult = functionResult pushText "string"

intResult :: Text -- ^ Description
          -> FunctionResults Int
intResult = functionResult (pushIntegral @Int) "integer"
