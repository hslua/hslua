--
-- Tests for the system module
--
local zip = require 'zip'
local tasty = require 'tasty'
local system = require 'system'

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
          local fh = io.open(filename, 'w')
          fh:write('Hi Mom!\n')
          fh:close()
          local archive = zip.create{filename}
          os.remove(filename)

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
      assert.are_equal(#zip.create().entries, 0)
    end),

    test('archive with file', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local fh = io.open(filename, 'w')
          fh:write('Hi Mom!\n')
          fh:close()
          local archive = zip.create{filename}
          assert.are_equal(
            archive.entries[1].path,
            'greetings.txt'
          )
        end)
      end)
    end)
  },

  group 'read_entry' {
    test('empty archive', function ()
      assert.are_equal(#zip.create().entries, 0)
    end),

    test('archive with file', function ()
      system.with_tmpdir('archive', function (tmpdir)
        system.with_wd(tmpdir, function ()
          local filename = 'greetings.txt'
          local fh = io.open(filename, 'w')
          fh:write('Hi Mom!\n')
          fh:close()
          local entry = zip.read_entry(filename)
          assert.are_equal(
            entry.path,
            'greetings.txt'
          )
        end)
      end)
    end)
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
