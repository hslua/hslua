{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module HsLuaSpec (tests) where

import Control.Monad
import Data.ByteString (ByteString)
import Foreign.Lua
import System.Mem (performMajorGC)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Instances ()

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Foreign.Lua as Lua
import qualified Test.QuickCheck.Monadic as QM

tests :: [TestTree]
tests =
  [ testGroup "StackValue"
    [ bytestring
    , bsShouldLive
    , listInstance
    , nulString
    ]
  , testGroup "Random StackValues"
    [ testProperty "can push/pop booleans" prop_bool
    , testProperty "can push/pop ints" prop_int
    , testProperty "can push/pop doubles" prop_double
    , testProperty "can push/pop bytestrings" prop_bytestring
    , testProperty "can push/pop lists of booleans" prop_lists_bool
    , testProperty "can push/pop lists of ints" prop_lists_int
    , testProperty "can push/pop lists of bytestrings" prop_lists_bytestring
    ]
  , testGroup "luaopen_* functions" $ map (uncurry testOpen)
    [ ("debug", opendebug)
    , ("io", openio)
    , ("math", openmath)
    , ("os", openos)
    , ("package", openpackage)
    , ("string", openstring)
    , ("table", opentable)
    ]
  , testGroup "luaopen_base returns the right number of tables" testOpenBase
  ]

bytestring :: TestTree
bytestring = testCase "ByteString -- unicode stuff" $ do
  let val = T.pack "öçşiğüİĞı"
  val' <- runLua $ do
    pushstring (T.encodeUtf8 val)
    T.decodeUtf8 `fmap` tostring 1
  assertEqual "Popped a different value or pop failed" val val'

bsShouldLive :: TestTree
bsShouldLive = testCase "ByteString should survive after GC/Lua destroyed" $ do
  (val, val') <- runLua $ do
    let val = B.pack "ByteString should survive"
    pushstring val
    val' <- tostring 1
    pop 1
    return (val, val')
  performMajorGC
  assertEqual "Popped a different value or pop failed" val val'

listInstance :: TestTree
listInstance = testCase "Push/pop StackValue lists" $ do
  let lst = [B.pack "first", B.pack "second"]
  runLua $ do
    push lst
    setglobal "mylist"
    size0 <- gettop
    liftIO $ assertEqual
      "After pushing the list and assigning to a variable, stack is not empty"
      0 size0
    getglobal "mylist"
    size1 <- gettop
    liftIO $ assertEqual "`getglobal` pushed more than one value to the stack" 1 size1
    lst' <- peek 1
    size2 <- gettop
    liftIO $ assertEqual "`tolist` left stuff on the stack" size1 size2
    liftIO $ assertEqual "Popped a different list or pop failed" lst lst'

nulString :: TestTree
nulString =
  testCase "String with NUL byte should be pushed/popped correctly" $ do
  let str = "A\NULB"
  str' <- runLua $ do
    pushstring (B.pack str)
    tostring 1
  assertEqual "Popped string is different than what's pushed" str (B.unpack str')

-----
-- Random Quickcheck testing for StackValue instances
-----

-- Bools
prop_bool :: Bool -> Property
prop_bool = testStackValueInstance
-- Ints
prop_int :: Int -> Property
prop_int = testStackValueInstance
-- Doubles
prop_double :: Double -> Property
prop_double = testStackValueInstance
-- Bytestrings
prop_bytestring :: ByteString -> Property
prop_bytestring = testStackValueInstance
-- Lists of bools
prop_lists_bool :: [Bool] -> Property
prop_lists_bool = testStackValueInstance
-- Lists of ints
prop_lists_int :: [Int] -> Property
prop_lists_int = testStackValueInstance
-- Lists of bytestrings
prop_lists_bytestring :: [ByteString] -> Property
prop_lists_bytestring = testStackValueInstance

-- Check that the StackValue instance for a datatype works
testStackValueInstance :: (Show t, Eq t, ToLuaStack t, FromLuaStack t) => t -> Property
testStackValueInstance t = QM.monadicIO $ do
  -- Init Lua state
  l <- QM.run newstate
  -- Get an ascending list of small (1-100) positive integers
  -- These are the indices at which we will push the value to be tested
  -- Note that duplicate values don't matter so we don't need to guard against that
  Ordered indices' <- QM.pick arbitrary
  let indices = map getPositive indices'
  let nItems = if null indices then 0 else last indices
  -- Make sure there's enough room in the stack
  QM.assert =<< QM.run (runLuaWith l $ checkstack nItems)
  -- Push elements
  QM.run $ forM_ [1..nItems] $ \n ->
    runLuaWith l $
    if n `elem` indices
      then push t
      else push n
  -- Check that the stack size is the same as the total number of pushed items
  stackSize <- QM.run $ fromIntegral `fmap` runLuaWith l gettop
  QM.assert $ stackSize == nItems
  -- Peek all items
  vals <- QM.run $ forM indices $ runLuaWith l . peek . StackIndex . fromIntegral
  -- Check that the stack size did not change after peeking
  newStackSize <- QM.run $ fromIntegral `fmap` runLuaWith l gettop
  QM.assert $ stackSize == newStackSize
  -- Check that we were able to peek at all pushed elements
  forM_ vals $ QM.assert . (== t)

  -- DEBUGGING -----------------------------------------------
  -- QM.run $ putStrLn $ "Testing value -------- " ++ show t
  -- QM.run $ putStrLn $ "Indices: " ++ show indices
  -- QM.run $ putStrLn $ "StackSize: " ++ show stackSize
  -- QM.run $ putStrLn $ "NewStackSize: " ++ show newStackSize
  -- QM.run $ putStrLn $ "Peeked Values: " ++ show vals

--------------------------------------------------------------------------------
-- luaopen_* functions

testOpen :: String -> Lua () -> TestTree
testOpen lib openfn = testCase ("open" ++ lib) $
  assertBool "opening the library failed" =<<
  runLua (openfn *> istable (-1))

testOpenBase :: [TestTree]
testOpenBase = (:[]) .
  testCase "openbase" $
  assertBool "loading base didn't push the expected number of tables" =<<
  (runLua $ do
    -- openbase returns one table in lua 5.2 and later, but two in 5.1
    openbase
    Lua.getglobal "_VERSION"
    version <- peek (-1) <* pop 1
    if version == ("Lua 5.1" :: ByteString)
      then liftM2 (&&) (istable (-1)) (istable (-2))
      else istable (-1))
