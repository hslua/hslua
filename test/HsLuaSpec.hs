module HsLuaSpec where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Mem (performMajorGC)

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import Scripting.Lua

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "StackValue" $ mapM_ fromHUnitTest
      [bytestring, bsShouldLive, listInstance, nulString]

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
