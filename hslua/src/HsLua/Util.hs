{-|
Module      : HsLua.Util
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

HsLua utility functions.
-}
module HsLua.Util
  ( getglobal'
  , setglobal'
  ) where

import Data.List (groupBy)
import HsLua.Core
  ( Lua, getfield, getglobal, nth, pop, pushvalue, remove, setfield
  , setglobal, top )

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
      pushvalue (nth 2)
      setfield (nth 2) lastField
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
  _ <- getglobal x
  mapM_ (\a -> getfield top a *> remove (nth 2)) xs
