{-
Copyright © 2018 Albert Krewinkel

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
Module      : Foreign.Lua.Utf8
Copyright   : © 2018 Albert Krewinkel
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
