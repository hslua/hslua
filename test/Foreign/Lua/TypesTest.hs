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
{-| Test that conversions from and to the lua stack are isomorphisms.
-}
module Foreign.Lua.TypesTest (tests) where

import Data.ByteString (ByteString)
import Foreign.Lua.Types
import Foreign.Lua.Functions
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Peek can act as left inverse of push"
  [ testProperty "booleans remain equal under push/peek"
    (prop_roundtripEqual :: Bool -> Property)

  , testProperty "ints remain equal under push/peek"
    (prop_roundtripEqual :: Int -> Property)

  , testProperty "lua numbers (i.e., doubles) remain equal under push/peek"
    (prop_roundtripEqual :: LuaNumber -> Property)

  , testProperty "lua integers remain equal under push/peek"
    (prop_roundtripEqual :: LuaInteger -> Property)

  , testProperty "bytestring remain equal under push/peek"
    (prop_roundtripEqual :: ByteString -> Property)

  , testProperty "lists of boolean remain equal under push/peeks"
    (prop_roundtripEqual :: [Bool] -> Property)

  , testProperty "lists of ints remain equal under push/peek"
    (prop_roundtripEqual :: [Int] -> Property)

  , testProperty "lists of bytestrings remain equal under push/peek"
    (prop_roundtripEqual :: [ByteString] -> Property)
  ]

prop_roundtripEqual :: (Eq a, FromLuaStack a, ToLuaStack a) => a -> Property
prop_roundtripEqual x = monadicIO $ do
  y <- run $ roundtrip x
  assert (x == y)

roundtrip :: (FromLuaStack a, ToLuaStack a) => a -> IO a
roundtrip x = runLua $ do
  push x
  res <- peek (-1)
  case res of
    Error err -> fail err
    Success y -> return y
