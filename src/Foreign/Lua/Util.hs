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
Portability : FlexibleInstances, ScopedTypeVariables

HsLua utility functions.
-}
module Foreign.Lua.Util
  ( getglobal'
  , returnError
  , runLua
  ) where

import Data.ByteString.Char8 (unpack)
import Data.List (groupBy)
import Foreign.Lua.Functions
import Foreign.Lua.Types

-- | Run lua computation using the default HsLua state as starting point.
runLua :: Lua a -> IO a
runLua lua = do
  st <- newstate
  res <- runLuaWith st lua
  liftIO (close st)
  return res

-- | Return an error, reading and removing the error message from the stack.
returnError :: Lua (Result a)
returnError = do
  err <- tostring (-1)
  return $ Error (unpack err)

-- | Like @getglobal@, but knows about packages. e. g.
--
-- > getglobal' l "math.sin"
--
-- returns correct result
getglobal' :: String -> Lua ()
getglobal' n = do
    getglobal x
    mapM_ dotable xs
  where
    (x : xs)  = splitdot n
    splitdot  = filter (/= ".") . groupBy (\a b -> a /= '.' && b /= '.')
    dotable a = getfield (-1) a *> gettop >>= \i -> remove (i - 1)
