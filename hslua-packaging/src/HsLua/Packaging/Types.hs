{-|
Module      : HsLua.Packaging.Types
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : Portable

Marshaling and documenting Haskell functions.
-}
module HsLua.Packaging.Types
  ( -- * Documented module
    Module (..)
  , Field (..)
    -- * Documented functions
  , DocumentedFunction (..)
    -- ** Documentation types
  , FunctionDoc (..)
  , ParameterDoc (..)
  , ResultsDoc (..)
  , ResultValueDoc (..)
  ) where

import Data.Text (Text)
import Data.Version (Version)
import HsLua.Core (LuaE, Name, NumResults)
import HsLua.ObjectOrientation (Operation)

-- | Named and documented Lua module.
data Module e = Module
  { moduleName :: Name
  , moduleDescription :: Text
  , moduleFields :: [Field e]
  , moduleFunctions :: [DocumentedFunction e]
  , moduleOperations :: [(Operation, DocumentedFunction e)]
  }

-- | Self-documenting module field
data Field e = Field
  { fieldName :: Text
  , fieldDescription :: Text
  , fieldPushValue :: LuaE e ()
  }

--
-- Function components
--

-- | Haskell equivallent to CFunction, i.e., function callable
-- from Lua.
data DocumentedFunction e = DocumentedFunction
  { callFunction :: LuaE e NumResults
  , functionName :: Name
  , functionDoc  :: FunctionDoc
  }

--
-- Documentation types
--

-- | Documentation for a Haskell function
data FunctionDoc = FunctionDoc
  { functionDescription :: Text
  , parameterDocs       :: [ParameterDoc]
  , functionResultsDocs :: ResultsDoc
  , functionSince       :: Maybe Version  -- ^ Version in which the function
                                          -- was introduced.
  }
  deriving (Eq, Ord, Show)

-- | Documentation for function parameters.
data ParameterDoc = ParameterDoc
  { parameterName :: Text
  , parameterType :: Text
  , parameterDescription :: Text
  , parameterIsOptional :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Documentation for the return values of a function.
data ResultsDoc
  = ResultsDocList [ResultValueDoc]  -- ^ List of individual results
  | ResultsDocMult Text              -- ^ Flexible results
  deriving (Eq, Ord, Show)

-- | Documentation for a single return value of a function.
data ResultValueDoc = ResultValueDoc
  { resultValueType :: Text
  , resultValueDescription :: Text
  }
  deriving (Eq, Ord, Show)
