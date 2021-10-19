{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.ObjectOrientation.Operation
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Binary and unary object operations.
-}
module HsLua.ObjectOrientation.Operation
  ( Operation (..)
  , metamethodName
  ) where

import HsLua.Core (Name)

-- | Lua metadata operation types.
data Operation
  = Add   -- ^ the addition (@+@) operation. If any operand for an
          -- addition is not a number (nor a string coercible to a
          -- number), Lua will try to call a metamethod. First, Lua will
          -- check the first operand (even if it is valid). If that
          -- operand does not define a metamethod for @__add@, then Lua
          -- will check the second operand. If Lua can find a
          -- metamethod, it calls the metamethod with the two operands
          -- as arguments, and the result of the call (adjusted to one
          -- value) is the result of the operation. Otherwise, it raises
          -- an error.
  | Sub   -- ^ the subtraction (@-@) operation. Behavior similar to the
          -- addition operation.
  | Mul   -- ^ the multiplication (@*@) operation. Behavior similar to the
          -- addition operation.
  | Div   -- ^ the division (@/@) operation. Behavior similar to the
          -- addition operation.
  | Mod   -- ^ the modulo (@%@) operation. Behavior similar to the
          -- addition operation.
  | Pow   -- ^ the exponentiation (@^@) operation. Behavior similar to the
          -- addition operation.
  | Unm   -- ^ the negation (unary @-@) operation. Behavior similar to the
          -- addition operation.
  | Idiv  -- ^ the floor division (@//@) operation. Behavior similar to
          -- the addition operation.
  | Band  -- ^ the bitwise AND (@&@) operation. Behavior similar to the
          -- addition operation, except that Lua will try a metamethod
          -- if any operand is neither an integer nor a value coercible
          -- to an integer (see §3.4.3).
  | Bor   -- ^ the bitwise OR (@|@) operation. Behavior similar to the
          -- bitwise AND operation.
  | Bxor  -- ^ the bitwise exclusive OR (binary @~@) operation. Behavior
          -- similar to the bitwise AND operation.
  | Bnot  -- ^ the bitwise NOT (unary @~@) operation. Behavior similar to
          -- the bitwise AND operation.
  | Shl   -- ^ the bitwise left shift (@<<@) operation. Behavior similar
          -- to the bitwise AND operation.
  | Shr   -- ^ the bitwise right shift (@>>@) operation. Behavior
          -- similar to the bitwise AND operation.
  | Concat
          -- ^ the concatenation (@..@) operation. Behavior similar to
          -- the addition operation, except that Lua will try a
          -- metamethod if any operand is neither a string nor a number
          -- (which is always coercible to a string).
  | Len   -- ^ the length (@#@) operation. If the object is not a string,
          -- Lua will try its metamethod. If there is a metamethod, Lua
          -- calls it with the object as argument, and the result of the
          -- call (always adjusted to one value) is the result of the
          -- operation. If there is no metamethod but the object is a
          -- table, then Lua uses the table length operation (see
          -- §3.4.7). Otherwise, Lua raises an error.
  | Eq    -- ^ the equal (@==@) operation. Behavior similar to the
          -- addition operation, except that Lua will try a metamethod
          -- only when the values being compared are either both tables
          -- or both full userdata and they are not primitively equal.
          -- The result of the call is always converted to a boolean.
  | Lt    -- ^ the less than (@<@) operation. Behavior similar to the
          -- addition operation, except that Lua will try a metamethod
          -- only when the values being compared are neither both
          -- numbers nor both strings. The result of the call is always
          -- converted to a boolean.
  | Le    -- ^ the less equal (@<=@) operation. Unlike other operations,
          -- the less-equal operation can use two different events.
          -- First, Lua looks for the @__le@ metamethod in both
          -- operands, like in the less than operation. If it cannot
          -- find such a metamethod, then it will try the @__lt@
          -- metamethod, assuming that a <= b is equivalent to not (b <
          -- a). As with the other comparison operators, the result is
          -- always a boolean. (This use of the @__lt@ event can be
          -- removed in future versions; it is also slower than a real
          -- __le metamethod.)
  | Index -- ^ The indexing access operation @table[key]@. This event
          -- happens when table is not a table or when key is not
          -- present in table. The metamethod is looked up in table.
  | Newindex
          -- ^ The indexing assignment @table[key] = value@. Like the
          -- index event, this event happens when table is not a table
          -- or when key is not present in table. The metamethod is
          -- looked up in table.
  | Call  -- ^ The call operation @func(args)@. This event happens when
          -- Lua tries to call a non-function value (that is, func is
          -- not a function). The metamethod is looked up in func. If
          -- present, the metamethod is called with func as its first
          -- argument, followed by the arguments of the original call
          -- (args). All results of the call are the result of the
          -- operation. (This is the only metamethod that allows
          -- multiple results.)
  | Tostring
          -- ^ The operation used to create a string representation of
          -- the object.
  | Pairs -- ^ the operation of iterating over the object's key-value
          -- pairs.
  | CustomOperation Name
          -- ^ a custom operation, with the metamethod name as
          -- parameter.
  deriving (Eq, Ord, Show)

-- | Returns the metamethod name used to control this operation.
metamethodName :: Operation -> Name
metamethodName = \case
  Add      -> "__add"
  Sub      -> "__sub"
  Mul      -> "__mul"
  Div      -> "__div"
  Mod      -> "__mod"
  Pow      -> "__pow"
  Unm      -> "__unm"
  Idiv     -> "__idiv"
  Band     -> "__band"
  Bor      -> "__bor"
  Bxor     -> "__bxor"
  Bnot     -> "__bnot"
  Shl      -> "__shl"
  Shr      -> "__shr"
  Concat   -> "__concat"
  Len      -> "__len"
  Eq       -> "__eq"
  Lt       -> "__lt"
  Le       -> "__le"
  Index    -> "__index"
  Newindex -> "__newindex"
  Call     -> "__call"
  Tostring -> "__tostring"
  Pairs    -> "__pairs"
  CustomOperation x -> x
