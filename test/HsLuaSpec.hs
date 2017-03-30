{-# LANGUAGE CPP #-}
module HsLuaSpec where

import Control.Monad (forM, forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Mem (performMajorGC)

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Monadic as QM

import Foreign.Lua

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "StackValue" $ mapM_ fromHUnitTest
      [bytestring, bsShouldLive, listInstance, nulString]
    describe "Random StackValues" $ do
      it "can push/pop booleans" $ property prop_bool
      it "can push/pop ints" $ property prop_int
      it "can push/pop doubles" $ property prop_double
      it "can push/pop bytestrings" $ property prop_bytestring
      it "can push/pop lists of booleans" $ property prop_lists_bool
      it "can push/pop lists of ints" $ property prop_lists_int
      it "can push/pop lists of bytestrings" $ property prop_lists_bytestring
    describe "luaopen_* functions" $ mapM_ fromHUnitTest $ map (uncurry testOpen) $
      [ ("table", opentable), ("io", openio), ("os", openos),
        ("string", openstring), ("math", openmath), ("debug", opendebug),
        ("package", openpackage) ]
    describe "luaopen_base returns two tables" $ fromHUnitTest $ testOpenBase

bytestring :: Test
bytestring = TestLabel "ByteString -- unicode stuff" $ TestCase $ do
    l <- newstate
    let val = T.pack "öçşiğüİĞı"
    pushstring l (T.encodeUtf8 val)
    val' <- T.decodeUtf8 `fmap` tostring l 1
    close l
    assertEqual "Popped a different value or pop failed" val val'

bsShouldLive :: Test
bsShouldLive = TestLabel "ByteString should survive after GC/Lua destroyed" $ TestCase $ do
    (val, val') <- do
      l <- newstate
      let val = B.pack "ByteString should survive"
      pushstring l val
      val' <- tostring l 1
      pop l 1
      close l
      return (val, val')
    performMajorGC
    assertEqual "Popped a different value or pop failed" val val'

listInstance :: Test
listInstance = TestLabel "Push/pop StackValue lists" $ TestCase $ do
    let lst = [B.pack "first", B.pack "second"]
    l <- newstate
    pushlist l lst
    setglobal l "mylist"
    size0 <- gettop l
    assertEqual
      "After pushing the list and assigning to a variable, stack is not empty"
      0 size0
    getglobal l "mylist"
    size1 <- gettop l
    assertEqual "`getglobal` pushed more than one value to the stack" 1 size1
    lst' <- tolist l 1
    size2 <- gettop l
    assertEqual "`tolist` left stuff on the stack" size1 size2
    close l
    assertEqual "Popped a different list or pop failed" (Just lst) lst'

nulString :: Test
nulString =
  TestLabel "String with NUL byte should be pushed/popped correctly" $ TestCase $ do
    l <- newstate
    let str = "A\NULB"
    pushstring l (B.pack str)
    str' <- tostring l 1
    close l
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
testStackValueInstance :: (Show t, Eq t, StackValue t) => t -> Property
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
  QM.assert =<< QM.run (checkstack l nItems)
  -- Push elements
  QM.run $ forM_ [1..nItems] $ \n ->
    if n `elem` indices
      then push l t
      else push l n
  -- Check that the stack size is the same as the total number of pushed items
  stackSize <- QM.run $ fromIntegral `fmap` gettop l
  QM.assert $ stackSize == nItems
  -- Peek all items
  vals <- QM.run $ forM indices $ peek l . StackIndex . fromIntegral
  -- Check that the stack size did not change after peeking
  newStackSize <- QM.run $ fromIntegral `fmap` gettop l
  QM.assert $ stackSize == newStackSize
  -- Check that we were able to peek at all pushed elements
  forM_ vals $ QM.assert . (== Just t)

  -- DEBUGGING -----------------------------------------------
  -- QM.run $ putStrLn $ "Testing value -------- " ++ show t
  -- QM.run $ putStrLn $ "Indices: " ++ show indices
  -- QM.run $ putStrLn $ "StackSize: " ++ show stackSize
  -- QM.run $ putStrLn $ "NewStackSize: " ++ show newStackSize
  -- QM.run $ putStrLn $ "Peeked Values: " ++ show vals

--------------------------------------------------------------------------------
-- luaopen_* functions

testOpen :: String -> (LuaState -> IO ())  -> Test
testOpen lib openfn = TestLabel ("open" ++ lib) . TestCase . assert $ do
    l <- newstate
    openfn l
    ret <- istable l (-1)
    close l
    return ret

testOpenBase :: Test
testOpenBase = TestLabel "openbase" . TestCase . assert $ do
    l <- newstate
    openbase l
    -- openbase returns one table in lua 5.2 and later
#if LUA_VERSION_NUMBER >= 502
    ret <- istable l (-1)
#else
    ret <- (&&) <$> istable l (-1) <*> istable l (-2)
#endif
    return ret

loadInspect :: LuaState -> Assertion
loadInspect l = do
    loadRet <- loadfile l "test/inspect.lua"
    assertEqual "load failed" 0 loadRet
    pcallRet <- pcall l 0 multret 0

    when (pcallRet /= 0) $ do
      msg <- tostring l 1
      close l
      assertFailure ("pcall failed with " ++ show pcallRet ++ "\n" ++
                     "error message was: " ++ B.unpack msg)

    setglobal l "inspect"
