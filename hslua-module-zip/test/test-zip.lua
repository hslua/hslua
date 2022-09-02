--
-- Tests for the system module
--
local zip = require 'zip'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

local empty_archive = '\80\75\5\6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0'
-- Check existence static fields
return {
  group 'create' {
    test('empty archive', function ()
      assert.are_equal(type(zip.create()), 'userdata')
    end),
  },

  group 'tobinary' {
    test('empty archive', function ()
      assert.are_equal(zip.create():tobinary(), empty_archive)
    end),
  },

  group 'toarchive' {
    test('empty archive', function ()
      assert.are_equal(type(zip.toarchive(empty_archive)), 'userdata')
    end),
    test('misformed archive', function ()
      assert.error_matches(
        function () zip.toarchive(empty_archive:sub(2)) end,
        ''
      )
    end),
  },
}
