Tasty Lua
=========

Write tests in Lua, integrate into tasty.

This package is not a traditional tasty test provider in that
tests are not run in Haskell. Instead, this package allows to
write and run tests in [Lua]; the results are then converted into
a format that allows to process the test results with Tasty. A
basic Lua test-suite is provided with the `tasty` Lua module.

Example
=======

Lua tests will usually be put into a separate file and then
loaded in the test program. The script must return the test
result tree:

``` lua
-- FILE: example-tests.lua
local tasty = require 'tasty'

local assert = tasty.assert

return {
  tasty.group 'examples  {
    tasty.test('multiplication', function() assert.are_equal(6, 2 * 3) end),
    tasty.test('truthyness', function() assert.is_truthy(0) end),
    tasty.group 'nil' {
      tasty.test('0 is nil', function () assert.is_nil(0) end)
    }
  }
}
```

On the Haskell side, the script is executed and the results are
included in the test results.

``` haskell
import Test.Tasty (defaultMain)
import Test.Tasty.Lua (testGroup)

main = do
  luaTest <- withCurrentDirectory "test" . Lua.run $ do
    -- run other commands to setup the Lua environment here.
    testsFromFile "example-tests.lua"

  defaultMain . testGroup "Haskell and Lua tests" $
    [ luaTest
    -- more tasty tests go here
    ]
```


[Lua]: https://lua.org/
