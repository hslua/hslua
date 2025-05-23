--
-- Tests for the system module
--
local io = require 'io'

local system = require 'system'
local tasty = require 'tasty'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

--- helper function, combining with_wd and with_tmpdir
function in_tmpdir (callback)
  return function ()
    system.with_tmpdir('test-sytem-tmpdir', function (tmpdir)
      system.with_wd(tmpdir, callback)
    end)
  end
end

--- dummy string for testing
local token = 'Banana'

--- check if token can be written into a file in given directory; returns the
--- content of this file.
function write_read_token (dir, filename)
  local filename = string.format('%s/%s', dir, 'foo.txt')
  local fh = io.open(filename, 'w')
  fh:write(token .. '\n')
  fh:close()
  return io.open(filename):read '*l'
end


-- Check existence static fields
return {
  group 'static fields' {
    test('arch', function ()
      assert.are_equal(type(system.arch), 'string')
    end),
    test('compiler_name', function ()
      assert.are_equal(type(system.compiler_name), 'string')
    end),
    test('compiler_version', function ()
      assert.are_equal(type(system.compiler_version), 'table')
    end),
    test('cputime_precision', function ()
      assert.are_equal(type(system.cputime_precision), 'number')
    end),
    test('os', function ()
      assert.are_equal(type(system.os), 'string')
    end),
  },

  group 'copy' {
    test('copys the file', in_tmpdir(function ()
      local content = 'Це тестовий контент.'
      local fh = io.open('a.txt', 'w')
      fh:write(content)
      fh:close()
      system.cp('a.txt', 'b.txt')
      assert.are_equal(content, io.open('b.txt'):read('a'))
    end))
  },

  group 'cputime' {
    test('returns a number', function ()
      assert.are_equal(type(system.cputime()), 'number')
    end),
  },

  group 'environment' {
    test('getenv returns same result as os.getenv', function ()
      assert.are_equal(system.getenv 'PATH', os.getenv 'PATH')
    end),

    test('setenv sets environment values', function ()
      system.setenv('HSLUA_SYSTEM_MODULE', 'test')
      -- apparently this works differently on Windows.
      local getenv = system.os == 'mingw32' and system.getenv or os.getenv
      assert.are_equal(getenv 'HSLUA_SYSTEM_MODULE', 'test')
    end),

  },
  group 'getwd' {
    test('returns a string', function ()
      assert.are_equal(type(system.getwd()), 'string')
    end)
  },

  group 'env' {
    test('returns a table', function ()
      assert.are_equal(type(system.env()), 'table')
    end)
  },

  group 'ls' {
    test('returns a table', function ()
      assert.are_equal(type(system.ls('.')), 'table')
    end),
    test('lists files in directory', in_tmpdir(function ()
      io.open('README.org', 'w'):close()
      assert.are_same(system.ls '.', {'README.org'})
    end)),
    test('argument defaults to `.`', function ()
      assert.are_equal(#system.ls('.'), #system.ls())
    end),
    test('fails when arg is not a directory', function ()
      assert.error_matches(
        function () system.ls('thisdoesnotexist') end,
        'thisdoesnotexist'
      )
      assert.error_matches(
        function () system.ls('README.md') end,
        'README%.md'
      )
    end)
  },

  group 'mkdir' {
    test('create directory', in_tmpdir(function ()
      system.mkdir 'foo'
      assert.are_equal((system.ls())[1], 'foo')
    end)),
    test('create nested directories', in_tmpdir(function ()
      system.mkdir('foo/bar', true)
      assert.are_equal((system.ls())[1], 'foo')
      assert.are_equal((system.ls 'foo')[1], 'bar')
    end)),
    test('cannot create existing directory', in_tmpdir(function ()
      assert.error_matches(function () system.mkdir '.' end, '%.')
    end)),
    test('optionally ignores existing directories', in_tmpdir(function ()
      system.mkdir 'foo'
      system.mkdir('foo', true)
    end)),
    test('normal operation', in_tmpdir(function () system.mkdir 'foo' end)),
  },

  group 'rm' {
    test('removes a file', in_tmpdir(function ()
      local fh = io.open('test.txt', 'w')
      fh:write('Hello\n')
      fh:close()
      system.rm('test.txt')
      assert.are_same(system.ls '.', {})
    end)),
    test('fails if file does not exist', in_tmpdir(function ()
      assert.error_matches(
        function () system.rm('nope.txt') end,
        'does not exist'
      )
    end)),
  },

  group 'rmdir' {
    test('remove empty directory', in_tmpdir(function ()
      system.mkdir 'remove-me'
      system.rmdir 'remove-me'
      assert.are_same(system.ls(), {})
    end)),
    test('fail if directory is not empty', in_tmpdir(function ()
      system.mkdir('outer/inner', true)
      assert.error_matches(function () system.rmdir('outer') end, '.')
    end)),
    test('optionally delete recursively', in_tmpdir(function ()
      system.mkdir('outer/inner', true)
      system.rmdir('outer', true)
      assert.are_same(system.ls(), {})
    end))
  },

  group 'tmpdirname' {
    test('returns a string', function ()
      assert.are_equal(type(system.tmpdirname()), 'string')
    end)
  },

  group 'with_env' {
    test('resets environment', function ()
      -- TODO: this test fails on Windows for unknown reasons and is
      -- disabled on there for that reason. This needs fixing.
      if system.os == 'mingw32' then return nil end

      local outer_value = 'outer test value'
      local inner_value = 'inner test value'
      local inner_only = 'test #2'

      function check_env ()
        assert.are_equal(os.getenv 'HSLUA_SYSTEM_TEST', inner_value)
        assert.are_equal(
          os.getenv 'HSLUA_SYSTEM_TEST_INNER_ONLY',
          inner_only
        )
        assert.is_nil(os.getenv 'HSLUA_SYSTEM_TEST_OUTER_ONLY')
      end

      local test_env = {
        HSLUA_SYSTEM_TEST = inner_value,
        HSLUA_SYSTEM_TEST_INNER_ONLY = inner_only
      }
      system.setenv('HSLUA_SYSTEM_TEST_OUTER_ONLY', outer_value)
      system.setenv('HSLUA_SYSTEM_TEST', outer_value)
      system.with_env(test_env, check_env)
      assert.are_equal(system.getenv 'HSLUA_SYSTEM_TEST', outer_value)
      assert.is_nil(system.getenv 'HSLUA_SYSTEM_TEST_INNER_ONLY')
      assert.are_equal(
        system.getenv 'HSLUA_SYSTEM_TEST_OUTER_ONLY',
        outer_value
      )
    end)
  },

  group 'with_tmpdir' {
    test('no base directory given', function ()
      assert.are_equal(system.with_tmpdir('foo', write_read_token), token)
    end),
    test('cwd as base directory', function ()
      assert.are_equal(system.with_tmpdir('.', 'foo', write_read_token), token)
    end),
  },

  group 'with_wd' {
    test('can change to test directory', function ()
      system.with_wd('test', function ()
        local cwd = system.getwd()
        assert.is_truthy(cwd:match 'test$')
      end)
    end),
    test('returns to old directory once done', function ()
      local cwd = system.getwd()
      system.with_wd('test', function () end)
      assert.are_equal(system.getwd(), cwd)
    end),
    test('working directory is passed to callback', function ()
      system.with_wd('test', function (path)
        assert.is_truthy(system.getwd():match (path .. '$'))
      end)
    end),
    test('all callback results are returned', function ()
      local a, b, c = system.with_wd('test', function (path)
        return 'a', 'b', 'c'
      end)
      assert.are_same({a, b, c}, {'a', 'b', 'c'})
    end),
    test('raises an error on nonexistent directory', function ()
      assert.error_matches(
        function ()
          system.with_wd('does-not-exist', function () end)
        end,
        'does not exist'
      )
    end)
  },

  group 'xdg' {
    test('returns a cache directory', function ()
      assert.is_truthy(#system.xdg('cache') > 1)
    end),
    test("second argument get's appended" , function ()
      local rel_path = 'pandoc/lua'
      local data_path = system.xdg('data', rel_path)
      assert.are_equal(
        -- replace backslashes with slashes to make this work on windows
        data_path:sub(- #rel_path, -1):gsub('\\', '/'),
        rel_path
      )
    end),
    test("raises an error if the XDG directory is unknown" , function ()
      assert.error_matches(function () system.xdg('foo') end, 'got: foo')
    end),
    test('`xdg_` prefix is accepted', function ()
      assert.is_truthy(#system.xdg('xdg_cache') > 1)
    end),
    test('returns a list of `XDG_DATA_DIRS`', function ()
      assert.are_equal(type(system.xdg('XDG_DATA_DIRS')), 'table')
    end),
  },
}
