{-
Copyright © 2017–2018 Albert Krewinkel

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
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Module.Text
Copyright   : © 2017–2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : ForeignFunctionInterface

Provide a lua module containing a selection of useful Text functions.
-}
module Foreign.Lua.Module.Text
  ( pushModuleText
  , preloadTextModule
  )where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Foreign.Lua (NumResults, Lua, Peekable, Pushable, ToHaskellFunction)
import qualified Foreign.Lua as Lua
import qualified Data.Text as T

-- | Pushes the @text@ module to the lua stack.
pushModuleText :: Lua NumResults
pushModuleText = do
  Lua.newtable
  addFunction "lower" (return . T.toLower :: Text -> Lua Text)
  addFunction "upper" (return . T.toUpper :: Text -> Lua Text)
  addFunction "reverse" (return . T.reverse :: Text -> Lua Text)
  addFunction "len" (return . fromIntegral . T.length :: Text -> Lua Lua.Integer)
  addFunction "sub" sub
  return 1

-- | Add the text module under the given name to the table of preloaded
-- packages.
preloadTextModule :: String -> Lua ()
preloadTextModule = flip addPackagePreloader pushModuleText

-- | Registers a preloading function. Takes an module name and the Lua operation
-- which produces the package.
addPackagePreloader :: String -> Lua NumResults -> Lua ()
addPackagePreloader name modulePusher = do
  Lua.getfield Lua.registryindex Lua.preloadTableRegistryField
  Lua.pushHaskellFunction modulePusher
  Lua.setfield (-2) name
  Lua.pop 1

-- | Attach a function to the table at the top of the stack, using the given
-- name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

-- | Returns a substring, using Lua's string indexing rules.
sub :: Text -> Lua.Integer -> Lua.Optional Lua.Integer -> Lua Text
sub s i j =
  let i' = fromIntegral i
      j' = fromIntegral . fromMaybe (-1) $ Lua.fromOptional j
      fromStart = if i' >= 0 then  i' - 1 else T.length s + i'
      fromEnd   = if j' <  0 then -j' - 1 else T.length s - j'
  in return . T.dropEnd fromEnd . T.drop fromStart $ s
