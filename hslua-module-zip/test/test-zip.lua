--
-- Tests for the system module
--
local zip = require 'zip'
local tasty = require 'tasty'
local system = require 'system'

local group = tasty.test_group
local test = tasty.test_case
local assert = tasty.assert

local make_sample_archive = function (opts)
  opts = opts or {}
  local filename = opts.filename or 'greetings.txt'
  local contents = opts.contents or 'Hello Bob!\n'
  return system.with_tmpdir('archive', function (tmpdir)
    return system.with_wd(tmpdir, function ()
      local fh = io.open(filename, 'w')
      fh:write(contents)
      fh:close()
      return zip.create{filename}
    end)
  end)
end

local empty_archive = '\80\75\5\6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0'
-- Check existence static fields
return {
  group 'Archive' {
    test('empty archive', function ()
      assert.are_equal(type(zip.Archive()), 'userdata')
    end),
  },

  group 'create' {
    test('archive with file', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local fh = io.open(filename, 'w')
          fh:write('Hi Mom!\n')
          fh:close()
          assert.are_equal(
            type(zip.create{filename}),
            'userdata'
          )
        end)
      end)
    end),


    test('recursive', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local dirname = 'greetings'
          local filename = dirname .. '/' .. 'french.txt'
          system.mkdir(dirname)
          local fh = io.open(filename, 'w')
          fh:write('Bonjour!\n')
          fh:close()
          local archive = zip.create({dirname}, {recursive=true})
          assert.are_equal(
            archive.entries[2].path,
            filename
          )
        end)
      end)
    end)
  },

  group 'extract' {
    test('archive with file', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local archive = make_sample_archive{
            filename = filename,
            contents = 'Hi Mom!\n',
          }
          archive:extract()

          assert.are_equal(system.ls()[1], filename)
          assert.are_equal(
            io.open(filename):read 'a',
            'Hi Mom!\n'
          )
        end)
      end)
    end)
  },

  group 'entries' {
    test('empty archive', function ()
      assert.are_equal(#zip.Archive().entries, 0)
    end),

    test('archive with file', function ()
      local archive = make_sample_archive{filename='greetings.txt'}
      assert.are_equal(
        archive.entries[1].path,
        'greetings.txt'
      )
    end),

    test('has type "ZipEntry list"', function ()
      local archive = make_sample_archive()
      assert.are_equal(getmetatable(archive.entries).__name, 'ZipEntry list')
    end),

    test('has `insert` method', function ()
      local archive = make_sample_archive()
      assert.are_equal(type(archive.entries.insert), 'function')
    end),
  },

  group 'read_entry' {
    test('has correct file path', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local fh = io.open(filename, 'w')
          fh:write('Hi Mom!\n')
          fh:close()
          local entry = zip.read_entry(filename)
          assert.are_equal(entry.path, filename)
        end)
      end)
    end),

    test('has contents', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local fh = io.open(filename, 'w')
          fh:write('Hallo!\n')
          fh:close()
          local entry = zip.read_entry(filename)
          assert.are_equal(
            entry:contents('foo'),
            'Hallo!\n'
          )
        end)
      end)
    end)
  },

  group 'tobytestring' {
    test('empty archive', function ()
      assert.are_equal(zip.Archive():tobytestring(), empty_archive)
    end),
  },

  group 'Archive constructor' {
    test('empty archive', function ()
      assert.are_equal(type(zip.Archive(empty_archive)), 'userdata')
    end),
    test('misformed archive', function ()
      assert.error_matches(
        function () zip.Archive(empty_archive:sub(2)) end,
        ''
      )
    end),
  },
}
