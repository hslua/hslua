{-
Copyright © 2017-2018 Albert Krewinkel

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
                2017-2018 Albert Krewinkel
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
  , Optional (Optional, fromOptional)
  ) where

import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Foreign.Lua.Core
import Foreign.Lua.Types

import qualified Data.ByteString as B
import qualified Foreign.Lua.Core as Lua

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
getglobal' :: ByteString -> Lua ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: ByteString -> Lua ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      pushvalue (nthFromTop 2)
      setfield (nthFromTop 2) lastField
      pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: ByteString -> [ByteString]
splitdot = filter (/= (B.singleton dot)) .
           B.groupBy (\a b -> a /= dot && b /= dot)
  where dot = fromIntegral (ord '.')

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: [ByteString] -> Lua ()
getnested [] = return ()
getnested (x:xs) = do
  getglobal x
  mapM_ (\a -> getfield stackTop a *> remove (nthFromTop 2)) xs

-- | Raise a Lua error, using the given value as the error object. This must be
-- the return value of a function which has been wrapped with
-- @'wrapHaskellFunction'@.
raiseError :: Pushable a => a -> Lua NumResults
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
  safePeek idx = do
    noValue <- isnoneornil idx
    if noValue
      then return . return $ Optional Nothing
      else fmap (Optional . Just) <$> safePeek idx

instance Pushable a => Pushable (Optional a) where
  push (Optional Nothing)  = pushnil
  push (Optional (Just x)) = push x
