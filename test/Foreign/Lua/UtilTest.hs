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
{-| Tests for utility types and functions
-}
module Foreign.Lua.UtilTest (tests) where

import Foreign.Lua
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Utilities"
  [ testCase "Optional return the value if it exists" . runLua $ do
      push "Moin"
      liftIO . assertEqual "Optional value should equal pushed value" (Just "Moin")
        =<< fmap fromOptional (peek stackTop)

  , testCase "Optional can deal with missing values" . runLua $ do
      pushnil
      liftIO . assertEqual "Peeking nil should return Nothing" Nothing
        =<< fmap fromOptional (peek stackTop :: Lua (Optional String))
      liftIO . assertEqual "Inexistant indices should be accepted" Nothing
        =<< fmap fromOptional (peek (nthFromBottom 9) :: Lua (Optional String))
  ]
