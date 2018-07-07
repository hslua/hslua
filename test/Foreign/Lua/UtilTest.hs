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
{-# LANGUAGE OverloadedStrings #-}
{-| Tests for utility types and functions
-}
module Foreign.Lua.UtilTest (tests) where

import Foreign.Lua
import Test.HsLua.Util ((=:), shouldBeResultOf, shouldBeErrorMessageOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Utilities"
  [ "Optional return the value if it exists" =:
    (Just "Moin" :: Maybe String) `shouldBeResultOf` do
      push ("Moin" :: String)
      fromOptional <$> peek stackTop

  , "Optional can deal with nil values" =:
    (Nothing :: Maybe String) `shouldBeResultOf` do
      pushnil
      fromOptional <$> peek stackTop

  , "Optional can deal with nonexistent (none) values" =:
    Nothing `shouldBeResultOf` do
      fmap fromOptional (peek (nthFromBottom 200) :: Lua (Optional String))

  , "raiseError causes a Lua error" =:
    "test error message" `shouldBeErrorMessageOf` do
      pushHaskellFunction (raiseError ("test error message" :: String))
      call 0 0
      return ()
  ]
