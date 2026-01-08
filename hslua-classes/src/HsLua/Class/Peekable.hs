{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Class.Peekable
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

Sending haskell objects to the lua stack.
-}
module HsLua.Class.Peekable
  ( Peekable (..)
  , peek
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import HsLua.Core as Lua
import HsLua.Marshalling
import Foreign.Ptr (Ptr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified HsLua.Marshalling.Peekers as Peekers

-- | A value that can be read from the Lua stack.
class Peekable a where
  -- | Function that retrieves a value from the Lua stack.
  safepeek :: LuaError e => Peeker e a

-- | Retrieves a 'Peekable' value from the stack. Throws an exception of
-- type @e@ if the given stack index does not a suitable value.
peek :: forall a e. (LuaError e, Peekable a) => StackIndex -> LuaE e a
peek = forcePeek . safepeek

instance Peekable () where
  safepeek = peekNil

instance Peekable Lua.Integer where
  safepeek = reportValueOnFailure "integer" tointeger

instance Peekable Lua.Number where
  safepeek = reportValueOnFailure "number" tonumber

instance Peekable B.ByteString where
  safepeek = peekByteString

instance Peekable Bool where
  safepeek = peekBool

instance Peekable CFunction where
  safepeek = reportValueOnFailure "C function" tocfunction

instance Peekable (Ptr a) where
  safepeek = reportValueOnFailure "userdata" touserdata

instance Peekable Lua.State where
  safepeek = reportValueOnFailure "Lua state (i.e., a thread)" tothread

instance Peekable Text where
  safepeek = peekText

instance Peekable BL.ByteString where
  safepeek = peekLazyByteString

instance Peekable Prelude.Integer where
  safepeek = peekIntegral

instance Peekable Int where
  safepeek = peekIntegral

instance Peekable Float where
  safepeek = peekRealFloat

instance Peekable Double where
  safepeek = peekRealFloat

instance {-# OVERLAPS #-} Peekable [Char] where
  safepeek = peekString

instance Peekable a => Peekable [a] where
  safepeek = peekList safepeek

instance (Ord a, Peekable a, Peekable b) => Peekable (Map a b) where
  safepeek = peekMap safepeek safepeek

instance (Ord a, Peekable a) => Peekable (Set a) where
  safepeek = peekSet safepeek

--
-- Tuples
--

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b) =>
  Peekable (a, b)
 where
  safepeek = peekPair safepeek safepeek

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c) =>
  Peekable (a, b, c)
 where
  safepeek = peekTriple safepeek safepeek safepeek

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c, Peekable d) =>
  Peekable (a, b, c, d)
 where
  safepeek = typeChecked "table" istable $ \idx ->
    (,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
          <*> nthValue idx 4

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c, Peekable d, Peekable e) =>
  Peekable (a, b, c, d, e)
 where
  safepeek = typeChecked "table" istable $ \idx ->
    (,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
           <*> nthValue idx 4 <*> nthValue idx 5

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c, Peekable d, Peekable e, Peekable f) =>
  Peekable (a, b, c, d, e, f)
 where
  safepeek = typeChecked "table" istable $ \idx ->
    (,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
            <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6


instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c, Peekable d,
   Peekable e, Peekable f, Peekable g) =>
  Peekable (a, b, c, d, e, f, g)
 where
  safepeek = typeChecked "table" istable $ \idx ->
    (,,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
             <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6
             <*> nthValue idx 7

instance {-# OVERLAPPABLE #-}
  (Peekable a, Peekable b, Peekable c, Peekable d,
   Peekable e, Peekable f, Peekable g, Peekable h) =>
  Peekable (a, b, c, d, e, f, g, h)
 where
  safepeek = typeChecked "table" istable $ \idx ->
    (,,,,,,,) <$> nthValue idx 1 <*> nthValue idx 2 <*> nthValue idx 3
              <*> nthValue idx 4 <*> nthValue idx 5 <*> nthValue idx 6
              <*> nthValue idx 7 <*> nthValue idx 8

-- | Helper function to get the nth table value
nthValue :: (LuaError e, Peekable a)
         => StackIndex -> Lua.Integer -> Peek e a
nthValue idx n = Peekers.peekIndexRaw n safepeek idx
