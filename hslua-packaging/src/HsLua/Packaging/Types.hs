{-|
Module      : HsLua.Packaging.Types
Copyright   : © 2020-2021 Albert Krewinkel
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
  , FunctionResultDoc (..)
  ) where

import Data.Text (Text)
import Data.Version (Version)
import HsLua.Core (LuaE, Name, NumResults)

-- | Named and documented Lua module.
data Module e = Module
  { moduleName :: Name
  , moduleDescription :: Text
  , moduleFields :: [Field e]
  , moduleFunctions :: [DocumentedFunction e]
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
  , functionResultDocs  :: [FunctionResultDoc]
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

-- | Documentation for the result of a function.
data FunctionResultDoc = FunctionResultDoc
  { functionResultType :: Text
  , functionResultDescription :: Text
  }
  deriving (Eq, Ord, Show)
