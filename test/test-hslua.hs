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
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified HsLuaTests
import qualified HsLua.CallTests
import qualified HsLua.CoreTests
import qualified HsLua.FunctionCallingTests
import qualified HsLua.ModuleTests
import qualified HsLua.PeekTests
import qualified HsLua.PushTests
import qualified HsLua.TypesTests
import qualified HsLua.Types.PeekableTests
import qualified HsLua.Types.PushableTests
import qualified HsLua.UserdataTests
import qualified HsLua.UtilTests

main :: IO ()
main = defaultMain $ testGroup "hslua" tests

-- | HSpec tests
tests :: [TestTree]
tests =
  [ HsLua.CoreTests.tests
  , HsLua.PushTests.tests
  , HsLua.FunctionCallingTests.tests
  , HsLua.UtilTests.tests
  , testGroup "Sending and receiving values from the stack"
    [ HsLua.TypesTests.tests
    , HsLua.Types.PeekableTests.tests
    , HsLua.Types.PushableTests.tests
    ]
  , HsLua.UserdataTests.tests
  , HsLua.ModuleTests.tests
  , HsLua.CallTests.tests
  , HsLuaTests.tests
  , HsLua.PeekTests.tests
  ]
