--
-- Tests for the system module
--
local path = require 'path'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

-- Check existence static fields
return {
  group 'static fields' {
    -- FIXME!!! these aren't working for some reason
    -- test('separator', function ()
    --   assert.are_equal(type(path.separator), 'string')
    -- end),
    -- test('search_path_separator', function ()
    --   assert.are_equal(type(path.search_path_separator), 'string')
    -- end),
  },

  group 'splitting/joining' {
    test('split_path is inverse of join', function ()
      local paths = {'one', 'two', 'test'}
      assert.are_same(path.split_path(path.join(paths)), paths)
    end),
  },
  group 'extensions' {
    test('drop_extensions', function ()
      assert.are_equal(path.drop_extensions 'image.jpg', 'image')
    end),
    test('has_extension', function ()
      assert.is_truthy(path.has_extension 'thesis.tex')
      assert.is_falsy(path.has_extension 'fstab')
    end),
    test('take_extensions', function ()
      assert.are_equal(path.take_extensions 'image.jpg', '.jpg')
    end),
  },

  -- group 'make_relative_path' {
  --   test('just the filename if file is within path', function()
  --     assert.are_equal(
  --       system.make_relative_path('/foo/bar/file.txt', '/foo/bar'),
  --       'file.txt'
  --     )
  --   end),
  --   test('no change if name outside of reference dir', function()
  --     assert.are_equal(
  --       system.make_relative_path('/foo/baz/file.txt', '/foo/bar'),
  --       '/foo/baz/file.txt'
  --     )
  --   end),
  --   test('base path defaults to current dir', function()
  --     assert.are_equal(
  --       system.make_relative_path(system.getwd() .. '/one/two.txt'),
  --       'one/two.txt'
  --     )
  --   end),
  --   test('return dot if both paths are the same', function()
  --     assert.are_equal(
  --       system.make_relative_path('/one/two/three', '/one/two/three/'),
  --       '.'
  --     )
  --   end)
  -- },
}
