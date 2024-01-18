{-|
Module      : HsLua.Util
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : beta
Portability : non-portable (depends on GHC)

HsLua utility functions.
-}
module HsLua.Util
  ( getglobal'
  , setglobal'
  ) where

import Data.List (groupBy)
import Data.String (IsString (fromString))
import HsLua.Core
  ( LuaE, LuaError (..), Name (..), getfield, getglobal, nth, pop
  , pushvalue, remove, setfield, setglobal, top )
import qualified HsLua.Core.Utf8 as Utf8

-- | Like @getglobal@, but knows about packages and nested tables. E.g.
--
-- > getglobal' "math.sin"
--
-- will return the function @sin@ in package @math@.
getglobal' :: LuaError e => Name -> LuaE e ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: LuaError e => Name -> LuaE e ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      pushvalue (nth 2)
      setfield (nth 2) lastField
      pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: Name -> [Name]
splitdot = map fromString
  . filter (/= ".")
  . groupBy (\a b -> a /= '.' && b /= '.')
  . Utf8.toString
  . fromName

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: LuaError e => [Name] -> LuaE e ()
getnested [] = return ()
getnested (x:xs) = do
  _ <- getglobal x
  mapM_ (\a -> getfield top a *> remove (nth 2)) xs
