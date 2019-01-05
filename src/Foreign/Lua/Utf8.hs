{-|
Module      : Foreign.Lua.Utf8
Copyright   : © 2018-2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

Encoding and decoding of String to and from UTF8.
-}
module Foreign.Lua.Utf8
  ( toString
  , toText
  , fromString
  , fromText
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding

-- | Decode @'ByteString'@ to @'String'@ using UTF-8.
toString :: ByteString -> String
toString = T.unpack . TextEncoding.decodeUtf8
{-# INLINABLE toString #-}

-- | Decode @'ByteString'@ to @'Text'@ using UTF-8.
toText :: ByteString -> Text
toText = TextEncoding.decodeUtf8
{-# INLINABLE toText #-}

-- | Encode @'String'@ to @'ByteString'@ using UTF-8.
fromString :: String -> ByteString
fromString = TextEncoding.encodeUtf8 . T.pack
{-# INLINABLE fromString #-}

-- | Encode @'Text'@ to @'ByteString'@ using UTF-8.
fromText :: Text -> ByteString
fromText = TextEncoding.encodeUtf8
{-# INLINABLE fromText #-}

    -- Text.unpack (Encoding.decodeUtf8With TextError.lenientDecode msg)
