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
import HsLua.Packaging
import qualified Data.Text as T

-- | The @text@ module.
documentedModule :: Module e
documentedModule = Module
  { moduleName = "text"
  , moduleOperations = []
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
  <#> textParam "s" "UTF-8 encoded string"
  =#> integralResult "length"
  #? "Determines the number of characters in a string."

-- | Wrapper for @'T.toLower'@.
lower :: DocumentedFunction e
lower = defun "lower"
  ### liftPure T.toLower
  <#> textParam "s" "UTF-8 string to convert to lowercase"
  =#> textResult "Lowercase copy of `s`"
  #? "Converts a string to lower case."

-- | Wrapper for @'T.reverse'@.
reverse :: DocumentedFunction e
reverse = defun "reverse"
  ### liftPure T.reverse
  <#> textParam "s" "UTF-8 string to revert"
  =#> textResult "Reversed `s`"
  #? "Reverses a string."

-- | Returns a substring, using Lua's string indexing rules.
sub :: DocumentedFunction e
sub = defun "sub"
  ### liftPure3 substring
  <#> textParam "s" "UTF-8 string"
  <#> textIndex "i" "substring start position"
  <#> opt (textIndex "j" "substring end position")
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
  <#> textParam "s" "UTF-8 string to convert to uppercase"
  =#> textResult "Uppercase copy of `s`"
  #? "Converts a string to upper case."

--
-- Parameters
--

-- | String index parameter
textIndex :: Text -- ^ parameter name
          -> Text -- ^ parameter description
          -> Parameter e Int
textIndex = integralParam @Int
