{-
Copyright © 2017 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-|
Module      : Foreign.Lua.Util
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

HsLua utility functions.
-}
module Foreign.Lua.Util
  ( getglobal'
  , setglobal'
  , runLua
  , runLuaEither
  , raiseError
  , OrNil (OrNil, toMaybe)
  , Optional (Optional, fromOptional)
  ) where

import Control.Exception (bracket, try)
import Data.List (groupBy)
import Foreign.Lua.Api
import Foreign.Lua.Types

-- | Run lua computation using the default HsLua state as starting point. Raised
-- exceptions are passed through; error handling is the responsibility of the
-- caller.
runLua :: Lua a -> IO a
runLua = (newstate `bracket` close) . flip runLuaWith

-- | Run the given Lua computation; exceptions raised in haskell code are
-- caught, but other exceptions (user exceptions raised in haskell, unchecked
-- type errors, etc.) are passed through.
runLuaEither :: Lua a -> IO (Either LuaException a)
runLuaEither = try . runLua

-- | Like @getglobal@, but knows about packages and nested tables. E.g.
--
-- > getglobal' "math.sin"
--
-- will return the function @sin@ in package @math@.
getglobal' :: String -> Lua ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: String -> Lua ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      pushvalue (-2)
      setfield (-2) lastField
      pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: String -> [String]
splitdot = filter (/= ".") . groupBy (\a b -> a /= '.' && b /= '.')

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: [String] -> Lua ()
getnested [] = return ()
getnested (x:xs) = do
  getglobal x
  mapM_ (\a -> getfield (-1) a *> remove (-2)) xs

-- | Raise a Lua error, using the given value as the error object. This must be
-- the return value of a function which has been wrapped with
-- @'wrapHaskellFunction'@.
raiseError :: ToLuaStack a => a -> Lua NumResults
raiseError e = do
  push e
  fromIntegral <$> lerror
{-# INLINABLE raiseError #-}

-- | Newtype wrapper intended to be used for optional Lua values. Nesting this
-- type is strongly discouraged as missing values on inner levels are
-- indistinguishable from missing values on an outer level; wrong values
-- would be the likely result.
newtype Optional a = Optional { fromOptional :: Maybe a }

instance FromLuaStack a => FromLuaStack (Optional a) where
  peek idx = do
    noValue <- isnoneornil idx
    if noValue
      then return (Optional Nothing)
      else Optional . Just <$> peek idx

instance ToLuaStack a => ToLuaStack (Optional a) where
  push (Optional Nothing)  = pushnil
  push (Optional (Just x)) = push x

-- | Like @'Optional'@, but deprecated. Will be removed in the next major
-- release.
newtype OrNil a = OrNil { toMaybe :: Maybe a }

instance FromLuaStack a => FromLuaStack (OrNil a) where
  peek idx = fmap (OrNil . fromOptional) (peek idx)

instance ToLuaStack a => ToLuaStack (OrNil a) where
  push (OrNil x)  = push (Optional x)
