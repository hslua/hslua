{-
Copyright © 2017-2020 Albert Krewinkel

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

import qualified Foreign.LuaTests
import qualified Foreign.Lua.CoreTests
import qualified Foreign.Lua.FunctionCallingTests
import qualified Foreign.Lua.ModuleTests
import qualified Foreign.Lua.PeekTests
import qualified Foreign.Lua.PushTests
import qualified Foreign.Lua.TypesTests
import qualified Foreign.Lua.Types.PeekableTests
import qualified Foreign.Lua.Types.PushableTests
import qualified Foreign.Lua.UserdataTests
import qualified Foreign.Lua.UtilTests

main :: IO ()
main = defaultMain $ testGroup "hslua" tests

-- | HSpec tests
tests :: [TestTree]
tests =
  [ Foreign.Lua.CoreTests.tests
  , Foreign.Lua.PushTests.tests
  , Foreign.Lua.FunctionCallingTests.tests
  , Foreign.Lua.UtilTests.tests
  , testGroup "Sendings and receiving values from the stack"
    [ Foreign.Lua.TypesTests.tests
    , Foreign.Lua.Types.PeekableTests.tests
    , Foreign.Lua.Types.PushableTests.tests
    ]
  , Foreign.Lua.UserdataTests.tests
  , Foreign.Lua.ModuleTests.tests
  , Foreign.LuaTests.tests
  , Foreign.Lua.PeekTests.tests
  ]
