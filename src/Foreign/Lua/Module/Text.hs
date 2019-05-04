{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Module.Text
Copyright   : © 2017–2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : ForeignFunctionInterface

Provide a lua module containing a selection of useful Text functions.
-}
module Foreign.Lua.Module.Text
  ( pushModule
  , pushModuleText
  , preloadTextModule
  )where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Foreign.Lua (NumResults, Lua, Peekable, Pushable, ToHaskellFunction)
import qualified Foreign.Lua as Lua
import qualified Data.Text as T

-- | Pushes the @text@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  Lua.addfunction "lower" (return . T.toLower :: Text -> Lua Text)
  Lua.addfunction "upper" (return . T.toUpper :: Text -> Lua Text)
  Lua.addfunction "reverse" (return . T.reverse :: Text -> Lua Text)
  Lua.addfunction "len" (return . fromIntegral . T.length :: Text -> Lua Lua.Integer)
  Lua.addfunction "sub" sub
  return 1

-- | Legacy alias for '@pushModule@'.
pushModuleText :: Lua NumResults
pushModuleText = pushModule

-- | Add the text module under the given name to the table of preloaded
-- packages.
preloadTextModule :: String -> Lua ()
preloadTextModule = flip Lua.preloadhs pushModule

-- | Returns a substring, using Lua's string indexing rules.
sub :: Text -> Lua.Integer -> Lua.Optional Lua.Integer -> Lua Text
sub s i j =
  let i' = fromIntegral i
      j' = fromIntegral . fromMaybe (-1) $ Lua.fromOptional j
      fromStart = if i' >= 0 then  i' - 1 else T.length s + i'
      fromEnd   = if j' <  0 then -j' - 1 else T.length s - j'
  in return . T.dropEnd fromEnd . T.drop fromStart $ s
