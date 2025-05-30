{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   :  © 2017-2024 Albert Krewinkel
License     :  MIT

Tests for Aeson–Lua glue.
-}
import Control.Monad (when)
import Data.Aeson (ToJSON, object, (.=))
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.Aeson
import Test.QuickCheck.Monadic (assert)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Monadic as QC

-- | Run this spec.
main :: IO ()
main = defaultMain tests

-- | Aeson tests
tests :: TestTree
tests = testGroup "hslua-aeson"
  [ testGroup "Value"
    [ testProperty "can be round-tripped through the stack" $
      assertRoundtripEqual pushValue peekValue . numbersToDoubles
    , testProperty "can roundtrip a bool nested in 50 layers of arrays" $
      \b -> QC.monadicIO $ do
        let go _ x = Aeson.Array $ Vector.fromList [x]
            mkValue a = foldr go (Aeson.Bool a) [(1::Int) .. 50]
        x <- QC.run . run @Lua.Exception $ do
          pushValue $ mkValue b
          forcePeek $ peekValue top
        return (x === mkValue b)
    , testProperty "can roundtrip a bool nested in 50 layers of objects" $
      \b -> QC.monadicIO $ do
        let go _ x = Aeson.Object $ KeyMap.fromList [("x", x)]
            mkValue a = foldr go (Aeson.Bool a) [(1::Int) .. 50]
        x <- QC.run . run @Lua.Exception $ do
          pushValue $ mkValue b
          forcePeek $ peekValue top
        return (x === mkValue b)
    , testProperty "can roundtrip a null nested in 50 layers of objects" $
      \() -> QC.monadicIO $ do
        let go _ x = Aeson.Object $ KeyMap.fromList [("x", x)]
            mkValue = foldr go Aeson.Null [(1::Int) .. 50]
        x <- QC.run . run @Lua.Exception $ do
          pushValue mkValue
          forcePeek $ peekValue top
        return (x === mkValue)
    ]

  , testGroup "via JSON"
    [ testProperty "can roundtrip 'Maybe Text' via JSON" $
      assertRoundtripEqual @(Maybe Int) pushViaJSON peekViaJSON
    , testProperty "can roundtrip '(Int, Float)' via JSON" $
      assertRoundtripEqual @(Int, Float) pushViaJSON peekViaJSON
    , testProperty "can roundtrip 'Either Bool Text' via JSON" $
      assertRoundtripEqual @(Either Bool Text) pushViaJSON peekViaJSON
    ]

  , testGroup "special encodings"
    [ testGroup "__toaeson"
      [ testCase "respect __toaeson metamethod" . run @Lua.Exception $ do
          pushTwentyThree TwentyThree
          val <- forcePeek $ peekValue top
          liftIO $ object [ "title" .= (23 :: Int) ] @?= val
      ]
    , testGroup "__tojson"
      [ testCase "respect __tojson metamethod" . run @Lua.Exception $ do
          newtable -- object

          newtable -- metatable
          pushHaskellFunction (1 <$ pushText "{\"answer\": 42}")
          setfield (nth 2) "__tojson"

          setmetatable (nth 2)
          val <- forcePeek $ peekValue top
          liftIO $ object [ "answer" .= (42 :: Int) ] @?= val
      ]
    ]
  ]

assertRoundtripEqual :: Eq a
                     => Pusher Lua.Exception a -> Peeker Lua.Exception a
                     -> a -> Property
assertRoundtripEqual pushX peekX x = QC.monadicIO $ do
  y <- QC.run $ roundtrip pushX peekX x
  assert (x == y)

roundtrip :: Pusher Lua.Exception a -> Peeker Lua.Exception a -> a -> IO a
roundtrip pushX peekX x = run $ do
  pushX x
  size <- gettop
  when (size /= 1) $
    failLua $ "not the right amount of elements on the stack: " ++ show size
  result <- forcePeek $ peekX top
  afterPeekSize <- gettop
  when (afterPeekSize /= 1) $
    failLua $ "peeking modified the stack: " ++ show afterPeekSize
  return result


-- | Ensure that numbers are representable as Doubles.
numbersToDoubles :: Aeson.Value -> Aeson.Value
numbersToDoubles (Aeson.Number x) = Aeson.Number . luaNumberToScientific $
                                    toRealFloat x
numbersToDoubles (Aeson.Object x) = Aeson.Object $ fmap numbersToDoubles x
numbersToDoubles (Aeson.Array x) = Aeson.Array $ fmap numbersToDoubles x
numbersToDoubles x = x

-- | Convert a Lua number to a scientific number, which are the basis for JSON
-- numbers.
luaNumberToScientific :: Lua.Number -> Scientific
luaNumberToScientific = fromFloatDigits . (realToFrac :: Lua.Number -> Double)

--
-- Type for __toaeson tests
--

-- | Example type with custom JSON encoding.
data TwentyThree = TwentyThree

instance ToJSON TwentyThree where
  toJSON _ = object
    [ "title" .= (23 :: Int)
    ]

peekTwentyThree :: Peeker e TwentyThree
peekTwentyThree =
  reportValueOnFailure "TwentyThree" (`Lua.fromuserdata` "TwentyThree")

pushTwentyThree :: LuaError e => Pusher e TwentyThree
pushTwentyThree _ = do
  Lua.newhsuserdatauv TwentyThree 0
  created <- Lua.newudmetatable "TwentyThree"
  when created $ do
    pushToAeson (fmap Aeson.toJSON . peekTwentyThree)
    setfield (nth 2) "__toaeson"
  setmetatable (nth 2)
