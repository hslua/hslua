{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Module.Text
Copyright   : © 2017–2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : ForeignFunctionInterface

Provides a Lua module containing a selection of useful Text functions.
-}
module HsLua.Module.Text
  ( -- * Module
    documentedModule
    -- ** Functions
  , len
  , lower
  , reverse
  , sub
  , upper
  ) where

import Prelude hiding (reverse)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import HsLua.Marshalling (peekIntegral, peekText, pushIntegral, pushText)
import HsLua.Packaging
import qualified Data.Text as T

-- | The @text@ module.
documentedModule :: Module e
documentedModule = Module
  { moduleName = "text"
  , moduleFields = []
  , moduleFunctions =
    [ len
    , lower
    , reverse
    , sub
    , upper
    ]
  , moduleDescription =
      "UTF-8 aware text manipulation functions, implemented in Haskell."
  }

--
-- Functions
--

-- | Wrapper for @'T.length'@.
len :: DocumentedFunction e
len = defun "len"
  ### liftPure T.length
  <#> textParam "s"
  =#> intResult "length"
  #? "Determines the number of characters in a string."

-- | Wrapper for @'T.toLower'@.
lower :: DocumentedFunction e
lower = defun "lower"
  ### liftPure T.toLower
  <#> textParam "s"
  =#> textResult "Lowercase copy of `s`"
  #? "Converts a string to lower case."

-- | Wrapper for @'T.reverse'@.
reverse :: DocumentedFunction e
reverse = defun "reverse"
  ### liftPure T.reverse
  <#> textParam "s"
  =#> textResult "Reversed `s`"
  #? "Reverses a string."

-- | Returns a substring, using Lua's string indexing rules.
sub :: DocumentedFunction e
sub = defun "sub"
  ### liftPure3 substring
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
upper :: DocumentedFunction e
upper = defun "upper"
  ### liftPure T.toUpper
  <#> textParam "s"
  =#> textResult "Uppercase copy of `s`"
  #? "Converts a string to upper case."

--
-- Parameters
--

textParam :: Text -> Parameter e Text
textParam name =
  parameter peekText "string" name "UTF-8 encoded string"

textIndex :: Text -> Text -> Parameter e Int
textIndex = parameter (peekIntegral @Int) "integer"

textOptionalIndex :: Text -> Text -> Parameter e (Maybe Int)
textOptionalIndex = optionalParameter (peekIntegral @Int) "integer"

--
-- Results
--

textResult :: Text -- ^ Description
           -> FunctionResults e Text
textResult = functionResult pushText "string"

intResult :: Text -- ^ Description
          -> FunctionResults e Int
intResult = functionResult (pushIntegral @Int) "integer"
