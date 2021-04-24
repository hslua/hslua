--
-- Tests for the text module
--
local text = require 'text'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

return {
  group 'len' {
    test('ASCII', function ()
      tasty.assert.are_equal(text.len 'five!', 5)
    end),
    test('German sz', function ()
      tasty.assert.are_equal(text.len 'Straße', 6)
    end),
    test('string with small letter e accute', function ()
      tasty.assert.are_equal(text.len 'Charité', 7)
    end),
    test('Unicode snowman', function ()
      tasty.assert.are_equal(text.len '☃', 1)
    end)
  },

  group 'lower' {
    test('uppercase ASCII', function ()
      assert.are_equal(text.lower 'YELLING', 'yelling')
    end),
    test('lowercase ASCII', function ()
      assert.are_equal(text.lower 'talking', 'talking')
    end),
    test('capitalized word with umlaut', function ()
      assert.are_equal(text.lower 'Lübeck', 'lübeck')
    end),
  },

  group 'upper' {
    test('uppercase ASCII', function ()
      assert.are_equal(text.upper 'YELLING', 'YELLING')
    end),
    test('lowercase ASCII', function ()
      assert.are_equal(text.upper 'silence', 'SILENCE')
    end),
    test('capitalized word with umlaut', function ()
      assert.are_equal(text.upper 'Lübeck', 'LÜBECK')
    end),
    test('German eszett becomes double S', function ()
      assert.are_equal(text.upper 'Spaß', 'SPASS')
    end),
  },

  group 'reverse' {
    test('reverse word with accent circumflex', function ()
      assert.are_equal(text.reverse 'être', 'ertê')
    end)
  },

  group 'sub' {
    test('behaves like string.sub for ASCII text', function ()
      local hw = 'Hello, World'
      assert.are_equal(text.sub(hw, 6),      string.sub(hw, 6))
      assert.are_equal(text.sub(hw, -1, -1), string.sub(hw, -1, -1))
      assert.are_equal(text.sub(hw, -7, -2), string.sub(hw, -7, -2))
      assert.are_equal(text.sub(hw,  7, -2), string.sub(hw, 7, -2))
      assert.are_equal(text.sub(hw,  1,  5), string.sub(hw, 1, 5))
      assert.are_equal(text.sub(hw,  5,  0), string.sub(hw, 5, 0))
      assert.are_equal(text.sub(hw,  0,  2), string.sub(hw, 0, 2))
      assert.are_equal(text.sub(hw, -19, 5), string.sub(hw, -19, 5))
    end),
    test('respects UTF-8', function ()
      assert.are_equal(text.sub('Für dich', 5), 'dich')
      assert.are_equal(text.sub('☢ radioactive', 0, 1), '☢')
      assert.are_equal(text.sub('☢ radioactive', -11, -1), 'radioactive')
    end)
  }
}
