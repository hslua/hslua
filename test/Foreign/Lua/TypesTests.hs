{-
Copyright © 2017-2021 Albert Krewinkel

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
module Foreign.Lua.TypesTests (tests) where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Foreign.Lua as Lua
import Test.HsLua.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic as QCMonadic
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Text as T

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "peek and push are well behaved"
  [ testGroup "Peek can act as left inverse of push"
    [ testProperty "round-tripping unit"
      (prop_roundtripEqual :: () -> Property)

    , testProperty "booleans remain equal under push/peek"
      (prop_roundtripEqual :: Bool -> Property)

    , testProperty "lua numbers (i.e., doubles) remain equal under push/peek"
      (prop_roundtripEqual :: Lua.Number -> Property)

    , testProperty "Lua integers remain equal under push/peek"
      (prop_roundtripEqual :: Lua.Integer -> Property)

    , testProperty "bytestring remain equal under push/peek"
      (prop_roundtripEqual :: ByteString -> Property)

    , testProperty "Prelude.Integer"
      (prop_roundtripEqual :: Prelude.Integer -> Property)

    , testProperty "Float"
      (prop_roundtripEqual :: Float -> Property)

    , testProperty "Double"
      (prop_roundtripEqual :: Double -> Property)

    , testProperty "round-tripping strings"
      (prop_roundtripEqual :: String -> Property)

    , testProperty "lists of boolean remain equal under push/peeks"
      (prop_roundtripEqual :: [Bool] -> Property)

    , testProperty "lists of lua integers remain equal under push/peek"
      (prop_roundtripEqual :: [Lua.Integer] -> Property)

    , testProperty "lists of bytestrings remain equal under push/peek"
      (prop_roundtripEqual :: [ByteString] -> Property)

    , testProperty "text"
      (prop_roundtripEqual :: T.Text -> Property)

    , testProperty "map of strings to Lua.Number"
      (prop_roundtripEqual :: Map String Lua.Number -> Property)

    , testProperty "set of strings"
      (prop_roundtripEqual :: Set Lua.Number -> Property)

    , testGroup "tuples"
      [ testProperty "pair of Lua.Numbers"
        (prop_roundtripEqual :: (Lua.Number, Lua.Number) -> Property)
      , testProperty "triple of Lua.Numbers"
        (prop_roundtripEqual :: (Lua.Number, Lua.Number, Lua.Number) -> Property)
      , testProperty "quadruple of Lua.Numbers"
        (prop_roundtripEqual
         :: (Lua.Number, Lua.Number, Lua.Number, Lua.Number) -> Property)
      , testProperty "quintuple of Lua.Numbers"
        (prop_roundtripEqual
         :: (Lua.Number, Lua.Number, Lua.Number, Lua.Number, Lua.Number) -> Property)
      , testProperty "hextuple of Text, Lua.Numbers and Booleans"
        (prop_roundtripEqual
         :: (Bool, Lua.Number, T.Text, Bool, Lua.Number, Lua.Number) -> Property)
      , testProperty "septuple of Text, Lua.Number and Booleans"
        (prop_roundtripEqual
         :: (T.Text, Bool, Lua.Number, Bool, Bool, Lua.Number, Bool) -> Property)
      , testProperty "octuple of Strings and Booleans"
        (prop_roundtripEqual
         :: (Bool, String, Bool, Bool, String, Bool, Bool, String) -> Property)
      ]
    ]

  , testGroup "Random stack values"
    [ testProperty "can push/pop booleans"
      (prop_stackPushingPulling :: Bool       -> Property)
    , testProperty "can push/pop lua integers"
      (prop_stackPushingPulling :: Lua.Integer -> Property)
    , testProperty "can push/pop lua numbers"
      (prop_stackPushingPulling :: Lua.Number  -> Property)
    , testProperty "can push/pop bytestrings"
      (prop_stackPushingPulling :: ByteString -> Property)
    , testProperty "can push/pop lists of booleans"
      (prop_stackPushingPulling :: [Bool]     -> Property)
    , testProperty "can push/pop lists of Lua.Integers"
      (prop_stackPushingPulling :: [Lua.Integer] -> Property)
    , testProperty "can push/pop lists of bytestrings"
      (prop_stackPushingPulling :: [ByteString] -> Property)
    , testProperty "can push/pop set of bytestrings"
      (prop_stackPushingPulling :: Set ByteString -> Property)
    ]
  ]

prop_roundtripEqual :: (Eq a, Peekable a, Pushable a) => a -> Property
prop_roundtripEqual x = monadicIO $ do
  y <- QCMonadic.run $ roundtrip x
  assert (x == y)

roundtrip :: (Peekable a, Pushable a) => a -> IO a
roundtrip x = Lua.run $ do
  push x
  peek (-1)

-- | More involved check that the Peekable and Pushable instances of a
-- datatype work
prop_stackPushingPulling :: (Eq t, Pushable t, Peekable t) => t -> Property
prop_stackPushingPulling t = monadicIO $ do
  -- Init Lua state
  l <- QCMonadic.run newstate
  -- Get an ascending list of small (1-100) positive integers
  -- These are the indices at which we will push the value to be tested
  -- Note that duplicate values don't matter so we don't need to guard against that
  Ordered indices' <- pick arbitrary
  let indices = map getPositive indices'
  let nItems = (if null indices then 0 else last indices) :: Lua.Integer
  -- Make sure there's enough room in the stack
  assert =<< QCMonadic.run (runWith l $ checkstack (2 * fromIntegral nItems))
  -- Push elements
  QCMonadic.run $ forM_ [1..nItems] $ \n ->
    runWith l $
    if n `elem` indices
      then push t
      else push n
  -- Check that the stack size is the same as the total number of pushed items
  stackSize <- QCMonadic.run $ runWith l gettop
  assert $ fromStackIndex stackSize == fromIntegral nItems
  -- Peek all items
  vals <- QCMonadic.run $ forM indices $ runWith l . peek . StackIndex . fromIntegral
  -- Check that the stack size did not change after peeking
  newStackSize <- QCMonadic.run $ runWith l gettop
  assert $ stackSize == newStackSize
  -- Check that we were able to peek at all pushed elements
  forM_ vals $ assert . (== t)
  -- Cleanup
  QCMonadic.run (close l)
