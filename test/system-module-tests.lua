--
-- Tests for the system module
--
local system = require 'system'

-- Check existence static fields
assert(type(system.arch) == 'string')
assert(type(system.compiler_name) == 'string')
assert(type(system.compiler_version) == 'table')
assert(type(system.os) == 'string')

-- getwd
assert(type(system.getwd()) == 'string')

-- env
assert(type(system.env()) == 'table')

-- ls
assert(type(system.ls('.')) == 'table')
assert(#system.ls('.') == #system.ls())
-- ls should fail when called on files or non-existent directories
assert(pcall(system.ls, 'thisdoesnotexist') == false)
assert(pcall(system.ls, 'README.md') == false)

-- mkdir and rmdir
function in_tmpdir (callback)
  local orig_dir = system.getwd()
  return system.with_tmpdir(
    'hello',
    function (tmpdir)
      system.chdir(tmpdir)
      local result = callback(tmpdir)
      system.chdir(orig_dir)
      return result
    end
  )
end

function test_mkdir_rmdir ()
  -- mkdir
  assert(not pcall(system.mkdir, '.'), "should not be possible to create `.`")
  assert(pcall(system.mkdir, 'foo'), "normal dir creation")
  assert(pcall(system.mkdir, 'foo', true), "dir creation if exists")
  assert((system.ls())[1] == 'foo')
  assert(not pcall(system.mkdir, 'bar/baz'),
         "creation of nested dir")
  assert(pcall(system.mkdir, 'bar/baz', true),
         "nested dir creation, including parent directories")
  assert((system.ls 'bar')[1] == 'baz')

  -- rmdir
  assert(pcall(system.rmdir, 'foo'), "delete empty directory")
  assert(not pcall(system.rmdir, 'bar'), "cannot delete non-empty dir")
  assert(pcall(system.rmdir, 'bar', true), "delete dir recursively")
  assert(#system.ls() == 0, "dir should be empty")
end
in_tmpdir(test_mkdir_rmdir)

-- tmpdirname
assert(type(system.tmpdirname()) == 'string', "tmpdirname should return a string")

-- with_env
local outer_value = 'outer test value'
local inner_value = 'inner test value'
local inner_only = 'test #2'

function check_env ()
  assert(os.getenv 'SYSTEM_TEST' == inner_value, "env has test value")
  assert(os.getenv 'SYSTEM_TEST_INNER_ONLY' == inner_only,
         "inner only exists")
  assert(os.getenv 'SYSTEM_TEST_OUTER_ONLY' == nil,
         "outer only variable should be unset")
end

local test_env = {
  SYSTEM_TEST = inner_value,
  SYSTEM_TEST_INNER_ONLY = inner_only
}
system.setenv('SYSTEM_TEST_OUTER_ONLY', outer_value)
system.setenv('SYSTEM_TEST', outer_value)
system.with_env(test_env, check_env)

assert(system.getenv 'SYSTEM_TEST' == outer_value, "value was restored")
assert(system.getenv 'SYSTEM_TEST_INNER_ONLY' == nil, "value was restored")
assert(system.getenv 'SYSTEM_TEST_OUTER_ONLY' == outer_value,
       "value was restored")

-- with_tmpdir
local token = 'Banana'
function write_read_token (tmpdir)
  local filename = tmpdir .. '/foo.txt'
  local fh = io.open(filename, 'w')
  fh:write(token .. '\n')
  fh:close()
  return io.open(filename):read '*l'
end

assert(system.with_tmpdir('.', 'foo', write_read_token) == token)
assert(system.with_tmpdir('foo', write_read_token) == token)


-- Complex scripts
function create_then_count_files ()
  io.open('README.org', 'w'):close()
  return #system.ls '.'
end

assert(in_tmpdir(create_then_count_files) == 1, 'Number of files should be 1')

system.setenv('TESTING', token)
assert(system.getenv 'TESTING' == token,
       'setting and getting env var is inconsistent')
