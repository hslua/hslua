{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-|
Module      : HsLua.Typing
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

The module provides Haskell types and values that can be used to
describe and declare the types of Lua values.
-}
module HsLua.Typing
  ( TypeSpec (..)
  , NamedType (..)
  , TypeDocs (..)
  , (#|#)
    -- * Types
  , anyType
  , voidType
    -- ** Built-in types
  , booleanType
  , functionType
  , integerType
  , lightUserdataType
  , nilType
  , numberType
  , stringType
  , tableType
  , threadType
  , userdataType
    -- ** Type constructors
  , seqType
  ) where

import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import HsLua.Core (Name)
import qualified HsLua.Core as HsLua
import qualified Data.Map as Map

-- | Type specification for Lua values.
data TypeSpec =
    AnyType                             -- ^ Unconstrained type.
  | NamedType NamedType                 -- ^ A type that's been given a name.
  | SequenceType TypeSpec               -- ^ Sequence of the given type.
  | SumType [TypeSpec]                  -- ^ Union type; a sum type.
  | RecordType (Map.Map Name TypeSpec)  -- ^ Record type (type product).
  | FunctionType [TypeSpec] [TypeSpec]  -- ^ Function type.
  deriving (Eq, Generic, Ord, Show)

-- | An atomic type; either a basic (built-in) type or a custom type.
data NamedType =
    BasicType HsLua.Type
  | CustomType TypeDocs
  deriving (Eq, Generic, Ord, Show)

-- | Documented custom type.
data TypeDocs = TypeDocs
  { typeName        :: Name
  , typeDescription :: Text
  , typeSpec        :: TypeSpec
  , typeRegistry    :: Maybe Name
  }
  deriving (Eq, Generic, Ord, Show)

-- | Returns the sum of two type specifiers, declaring that a Lua value
-- can have either type.
(#|#) :: TypeSpec -> TypeSpec -> TypeSpec
SumType a #|# SumType b = SumType (a ++ b)
_         #|# _         = AnyType

--
-- Built-in types
--

-- | Unconstraint type; any Lua value.
anyType :: TypeSpec
anyType = AnyType

-- | A type for which there cannot be any value.
voidType :: TypeSpec
voidType = SumType []

-- | The built-in @boolean@ Lua type.
booleanType :: TypeSpec
booleanType = NamedType (BasicType HsLua.TypeBoolean)

-- | The built-in @function@ Lua type.
functionType :: TypeSpec
functionType = NamedType (BasicType HsLua.TypeFunction)

-- | The built-in @number@ Lua type.
integerType :: TypeSpec
integerType = NamedType (BasicType HsLua.TypeNumber)

-- | The built-in @light userdata@ Lua type.
lightUserdataType :: TypeSpec
lightUserdataType = NamedType (BasicType HsLua.TypeLightUserdata)

-- | The built-in @nil@ Lua type.
nilType :: TypeSpec
nilType = NamedType (BasicType HsLua.TypeNil)

-- | The built-in @number@ Lua type.
numberType :: TypeSpec
numberType = NamedType (BasicType HsLua.TypeNumber)

-- | The built-in @string@ Lua type.
stringType :: TypeSpec
stringType = NamedType (BasicType HsLua.TypeString)

-- | The built-in @table@ Lua type.
tableType :: TypeSpec
tableType = NamedType (BasicType HsLua.TypeTable)

-- | The built-in @thread@ Lua type.
threadType :: TypeSpec
threadType = NamedType (BasicType HsLua.TypeThread)

-- | The built-in @userdata@ Lua type.
userdataType :: TypeSpec
userdataType = NamedType (BasicType HsLua.TypeUserdata)

-- | For backwards compatibility and convenience, strings can be used as
-- TypeSpec values.
instance IsString TypeSpec where
  fromString = \case
    "any"            -> anyType
    "boolean"        -> booleanType
    "function"       -> functionType
    "integer"        -> numberType
    "light userdata" -> lightUserdataType
    "nil"            -> nilType
    "number"         -> numberType
    "string"         -> stringType
    "table"          -> tableType
    "userdata"       -> userdataType
    s -> NamedType $ CustomType $ TypeDocs
         { typeName = fromString s
         , typeSpec = anyType
         , typeDescription = mempty
         , typeRegistry = Nothing
         }

--
-- Constructors
--

seqType :: TypeSpec -> TypeSpec
seqType = SequenceType
