local tasty = require 'tasty'

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  group 'assertors'  {
    group 'is_truthy' {
      test('zero is truthy', function() assert.is_truthy(0) end),
      test('true is truthy', function() assert.is_truthy(true) end),
      test('empty string is truthy', function() assert.is_truthy '' end),
    },
    group 'is_falsy' {
      test('false is falsy', function() assert.is_falsy(false) end),
      test('nil is falsy', function() assert.is_falsy(nil) end),
    },
    group 'is_nil' {
      test('nil is nil', function () assert.is_nil(nil) end)
    },
    group 'are_equal' {
      test('equal strings', function () assert.are_equal('test', 'test') end)
    }
  },
}
