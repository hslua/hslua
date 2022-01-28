---
layout: default
author: Albert Krewinkel
title: tasty-lua
subtitle: Write tests in Lua, integrate into tasty.
date: 2021-06-16
---

[![Hackage]](https://hackage.haskell.org/package/tasty-lua)

[Hackage]: https://img.shields.io/hackage/v/tasty-lua.svg

Overview
--------

This package is not a traditional tasty test provider in that
tests are not run in Haskell. Instead, this package allows to
write and run tests in [Lua][]; the results are then converted
into a format that allows to process the test results with Tasty.
A basic Lua test-suite is provided with the `tasty` Lua module.

Example
-------

Tasty Lua scripts can be put into a separate file and then loaded
in the test program. The script must return the test result tree:

``` lua
-- FILE: example-tests.lua
local tasty = require 'tasty'

local assert = tasty.assert

return {
  tasty.group 'examples'  {
    tasty.test('multiplication', function() assert.are_equal(6, 2 * 3) end),
    tasty.test('truthyness', function() assert.is_truthy(0) end),
    tasty.group 'nil' {
      tasty.test('0 is nil', function () assert.is_nil(0) end)
    }
  }
}
```

On the Haskell side, the script is executed and the results are
included in the test results. Two ways of integrating the tests
into the test output are supported.

### One test per file

This method is closest to Tasty's intended way of running tests. A
script is run as a single test case. On success, the number of
passing Lua tests is included in the output. In the case of a
failure, all failure information is collected and presented to the
user.

``` haskell
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Lua (testLuaFile)
import Foreign.Lua (run)

main = defaultMain $
    testLuaFile run "Lua example tests" "example-tests.lua"
```

### Lua tests as Tasty tests

Lua tests can be transformed into mock Tasty tests, thus showing
all tests with their status in the final output.

``` haskell
import Foreign.Lua (run)
import System.Directory (withCurrentDirectory)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Lua (translateResultsFromFile)

main = do
  luaTest <- withCurrentDirectory "test" . run $ do
    -- run other commands to setup the Lua environment here.
    translateResultsFromFile "example-tests.lua"

  defaultMain . testGroup "Haskell and Lua tests" $
    [ luaTest
    -- more tasty tests go here
    ]
```


[Lua]: https://lua.org/
