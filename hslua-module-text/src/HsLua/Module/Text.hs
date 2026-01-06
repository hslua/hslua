{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-|
Module      : HsLua.Module.Text
Copyright   : © 2017–2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : GHC only

Provides a Lua module containing a selection of useful Text functions.
-}
module HsLua.Module.Text
  ( -- * Module
    documentedModule
    -- ** Functions
  , fromencoding
  , len
  , lower
  , reverse
  , sub
  , toencoding
  , upper
  ) where

import Prelude hiding (reverse)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc (alloca)
import HsLua.Core (LuaError)
import HsLua.Packaging
import Lua (lua_pushlstring, lua_tolstring)
import System.IO.Error (tryIOError)
import qualified Data.Text as T
import qualified Foreign.Storable as F
import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as GHC
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua

-- | The @text@ module.
documentedModule :: LuaError e => Module e
documentedModule = defmodule "text"
  `withFunctions`
    [ fromencoding
    , len
    , lower
    , reverse
    , sub
    , toencoding
    , upper
    ]
  `withDescription`
      "UTF-8 aware text manipulation functions, implemented in Haskell."

--
-- Functions
--

-- | Recodes a string as UTF-8.
fromencoding :: LuaError e => DocumentedFunction e
fromencoding = defun "fromencoding"
  ### (\strIdx menc -> do
          l <- Lua.state
          result <- Lua.liftIO . tryIOError $ do
            encoding <- maybe getFileSystemEncoding GHC.mkTextEncoding menc
            alloca $ \lenPtr -> do
              cstr <- lua_tolstring l strIdx lenPtr
              -- cstr cannot be NULL, or stringIndex would have failed.
              cstrLen <- F.peek lenPtr
              GHC.peekCStringLen encoding (cstr, fromIntegral cstrLen)
          case result of
            Right s -> pure $ T.pack s
            Left err -> Lua.failLua (show err))
  <#> parameter stringIndex "string" "s" "string to be converted"
  <#> opt (stringParam "encoding" "target encoding")
  =#> functionResult Lua.pushText "string" "UTF-8 string"
  #? T.unlines
     [ "Converts a string to UTF-8. The `encoding` parameter specifies the"
     , "encoding of the input string. On Windows, that parameter defaults"
     , "to the current ANSI code page; on other platforms the function"
     , "will try to use the file system's encoding."
     , ""
     , "The set of known encodings is system dependent, but includes at"
     , "least `UTF-8`, `UTF-16BE`, `UTF-16LE`, `UTF-32BE`, and `UTF-32LE`."
     , "Note that the default code page on Windows is available through"
     , "`CP0`."
     ]
  where
    stringIndex idx = do
      isstr <- Lua.liftLua (Lua.isstring idx)
      if isstr
        then pure idx
        else Lua.typeMismatchMessage "string" idx >>= Lua.failPeek

-- | Wrapper for @'T.length'@.
len :: DocumentedFunction e
len = defun "len"
  ### liftPure T.length
  <#> textParam "s" "UTF-8 encoded string"
  =#> integralResult "length"
  #? "Returns the length of a UTF-8 string, i.e., the number of characters."

-- | Wrapper for @'T.toLower'@.
lower :: DocumentedFunction e
lower = defun "lower"
  ### liftPure T.toLower
  <#> textParam "s" "UTF-8 string to convert to lowercase"
  =#> textResult "Lowercase copy of `s`"
  #? "Returns a copy of a UTF-8 string, converted to lowercase."

-- | Wrapper for @'T.reverse'@.
reverse :: DocumentedFunction e
reverse = defun "reverse"
  ### liftPure T.reverse
  <#> textParam "s" "UTF-8 string to revert"
  =#> textResult "Reversed `s`"
  #? "Returns a copy of a UTF-8 string, with characters reversed."

-- | Returns a substring, using Lua's string indexing rules.
sub :: DocumentedFunction e
sub = defun "sub"
  ### liftPure3 substring
  <#> textParam "s" "UTF-8 string"
  <#> textIndex "i" "substring start position"
  <#> opt (textIndex "j" "substring end position")
  =#> textResult "text substring"
  #? T.unlines
     [ "Returns a substring of a UTF-8 string, using Lua's string"
     , "indexing rules."
     ]
  where
    substring :: Text -> Int -> Maybe Int -> Text
    substring s i jopt =
      let j = fromMaybe (-1) jopt
          fromStart = if i >= 0 then  i - 1 else T.length s + i
          fromEnd   = if j <  0 then -j - 1 else T.length s - j
      in T.dropEnd fromEnd . T.drop fromStart $ s

-- | Converts a UTF-8 string to a different encoding.
toencoding :: LuaError e => DocumentedFunction e
toencoding = defun "toencoding"
  ### (\s menc -> do
          l <- Lua.state
          result <- Lua.liftIO . tryIOError $ do
            encoding <- maybe getFileSystemEncoding GHC.mkTextEncoding menc
            GHC.withCStringLen encoding (T.unpack s) $ \(sPtr, sLen) ->
              lua_pushlstring l sPtr (fromIntegral sLen)
          case result of
            Right () -> pure ()
            Left err -> Lua.failLua (show err))
  <#> textParam "s" "UTF-8 string"
  <#> opt (stringParam "enc" "target encoding")
  =#> functionResult (const (pure ())) "string" "re-encoded string"
  #? T.unlines
     [ "Converts a UTF-8 string to a different encoding. The `encoding`"
     , "parameter defaults to the current ANSI code page on Windows; on"
     , "other platforms it will try to guess the file system's encoding."
     , ""
     , "The set of known encodings is system dependent, but includes at"
     , "least `UTF-8`, `UTF-16BE`, `UTF-16LE`, `UTF-32BE`, and `UTF-32LE`."
     , "Note that the default code page on Windows is available through"
     , "`CP0`."
     ]

-- | Wrapper for @'T.toUpper'@.
upper :: DocumentedFunction e
upper = defun "upper"
  ### liftPure T.toUpper
  <#> textParam "s" "UTF-8 string to convert to uppercase"
  =#> textResult "Uppercase copy of `s`"
  #? "Returns a copy of a UTF-8 string, converted to uppercase."

--
-- Parameters
--

-- | String index parameter
textIndex :: Text -- ^ parameter name
          -> Text -- ^ parameter description
          -> Parameter e Int
textIndex = integralParam @Int

--
-- Helpers
--
getFileSystemEncoding :: IO GHC.TextEncoding
getFileSystemEncoding =
#if defined(mingw32_HOST_OS)
  GHC.mkTextEncoding "CP0"  -- a.k.a CP_ACP
#else
  GHC.getFileSystemEncoding
#endif
