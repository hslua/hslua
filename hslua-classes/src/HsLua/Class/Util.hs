{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Class.Util
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

HsLua utility functions.
-}
module HsLua.Class.Util
  ( getglobal'
  , setglobal'
  , raiseError
  , Optional (Optional, fromOptional)
    -- * getting values
  , peekEither
  , peekRead
  , popValue
  ) where

import Data.List (groupBy)
import HsLua.Core (LuaE, LuaError, NumResults, StackIndex, nth, top)
import HsLua.Class.Peekable (Peekable (peek), PeekError (exceptionFromMessage))
import HsLua.Class.Pushable (Pushable (push))
import Text.Read (readMaybe)

import qualified Control.Monad.Catch as Catch
import qualified HsLua.Core as Lua

-- | Like @getglobal@, but knows about packages and nested tables. E.g.
--
-- > getglobal' "math.sin"
--
-- will return the function @sin@ in package @math@.
getglobal' :: LuaError e => String -> LuaE e ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: LuaError e => String -> LuaE e ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      Lua.setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      Lua.pushvalue (nth 2)
      Lua.setfield (nth 2) lastField
      Lua.pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: String -> [String]
splitdot = filter (/= ".") . groupBy (\a b -> a /= '.' && b /= '.')

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: LuaError e => [String] -> LuaE e ()
getnested [] = return ()
getnested (x:xs) = do
  _ <- Lua.getglobal x
  mapM_ (\a -> Lua.getfield top a *> Lua.remove (nth 2)) xs

-- | Raise a Lua error, using the given value as the error object.
raiseError :: (PeekError e, Pushable a) => a -> LuaE e NumResults
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
  peek idx = do
    noValue <- Lua.isnoneornil idx
    if noValue
      then return $ Optional Nothing
      else Optional . Just <$> peek idx

instance Pushable a => Pushable (Optional a) where
  push (Optional Nothing)  = Lua.pushnil
  push (Optional (Just x)) = push x


--
-- Getting Values
--

-- | Get a value by retrieving a String from Lua, then using @'readMaybe'@ to
-- convert the String into a Haskell value.
peekRead :: forall e a. (PeekError e, Read a)
         => StackIndex -> LuaE e a
peekRead idx = do
  s <- peek idx
  case readMaybe s of
    Just x -> return x
    Nothing -> Catch.throwM $ exceptionFromMessage @e ("Could not read: " ++ s)

-- | Try to convert the value at the given stack index to a Haskell value.
-- Returns 'Left' with the error on failure.
peekEither :: (PeekError e, Peekable a)
           => StackIndex -> LuaE e (Either e a)
peekEither = Lua.try . peek

-- | Get, then pop the value at the top of the stack. The pop operation is
-- executed even if the retrieval operation failed.
popValue :: (PeekError e, Peekable a) => LuaE e a
popValue = peek top `Catch.finally` Lua.pop 1
{-# INLINABLE popValue #-}