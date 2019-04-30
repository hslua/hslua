--
-- Tests for the hstext module
--
local system = require 'system'

-- Check existence static fields
assert(type(system.arch) == 'string')
assert(type(system.compiler_name) == 'string')
assert(type(system.compiler_version) == 'table')
assert(type(system.os) == 'string')

local token = 'Banana'
function write_read_token (tmpdir)
  local filename = tmpdir .. '/foo.txt'
  local fh = io.open(filename, 'w')
  fh:write(token .. '\n')
  fh:close()
  return io.open(filename):read '*l'
end

-- with_tmpdir
assert(system.with_tmpdir('.', 'foo', write_read_token) == token)
assert(system.with_tmpdir('foo', write_read_token) == token)

-- tmpdirname
assert(type(system.tmpdirname()) == 'string', "tmpdirname should return a string")

-- env
assert(type(system.env()) == 'table')

-- ls
assert(type(system.ls('.')) == 'table')
assert(#system.ls('.') == #system.ls())
-- ls should fail when called on files or non-existent directories
assert(pcall(system.ls, 'thisdoesnotexist') == false)
assert(pcall(system.ls, 'README.md') == false)

-- currentdir
assert(type(system.currentdir()) == 'string')
-- pwd is an alias for currentdir
assert(system.currentdir() == system.pwd())


-- Complex scripts
function in_tmpdir (callback)
  local orig_dir = system.currentdir()
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

function create_then_count_files ()
  io.open('README.org', 'w'):close()
  return #system.ls '.'
end

assert(in_tmpdir(create_then_count_files) == 1, 'Number of files should be 1')

system.setenv('TESTING', token)
assert(system.getenv 'TESTING' == token,
       'setting and getting env var is inconsistent')
