{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Class.Util
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

HsLua utility functions.
-}
module HsLua.Class.Util
  ( raiseError
  , Optional (Optional, fromOptional)
    -- * getting values
  , peekEither
  , popValue
  ) where

import Control.Applicative ((<|>))
import HsLua.Core (LuaE, LuaError, NumResults, StackIndex, top)
import HsLua.Class.Peekable (Peekable (safepeek), peek)
import HsLua.Class.Pushable (Pushable (push))

import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua

-- | Raise a Lua error, using the given value as the error object.
raiseError :: (LuaError e, Pushable a) => a -> LuaE e NumResults
raiseError e = do
  push e
  Lua.error
{-# INLINABLE raiseError #-}

-- | Newtype wrapper intended to be used for optional Lua values. Nesting this
-- type is strongly discouraged as missing values on inner levels are
-- indistinguishable from missing values on an outer level; wrong values
-- would be the likely result.
newtype Optional a = Optional { fromOptional :: Maybe a }

instance Peekable a => Peekable (Optional a) where
  safepeek idx = (Optional Nothing <$ Lua.peekNoneOrNil idx)
             <|> (Optional . Just <$> safepeek idx)

instance Pushable a => Pushable (Optional a) where
  push (Optional Nothing)  = Lua.pushnil
  push (Optional (Just x)) = push x


--
-- Getting Values
--

-- | Try to convert the value at the given stack index to a Haskell value.
-- Returns 'Left' with the error on failure.
peekEither :: (LuaError e, Peekable a)
           => StackIndex -> LuaE e (Either e a)
peekEither = Lua.try . peek

-- | Get, then pop the value at the top of the stack. The pop operation is
-- executed even if the retrieval operation failed.
popValue :: (LuaError e, Peekable a) => LuaE e a
popValue = Lua.forcePeek $ safepeek top `Lua.lastly` Lua.pop 1
{-# INLINABLE popValue #-}
