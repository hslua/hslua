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
    test('separator', function ()
      assert.are_equal(type(path.separator), 'string')
    end),
    test('search_path_separator', function ()
      assert.are_equal(type(path.search_path_separator), 'string')
    end),
  },

  group 'directory' {
    test('directory from file path', function ()
      print(path.join{'/', '2020', 'img', 'mask.jpg'})
      assert.are_equal(
        path.directory(path.join{'/', '2020', 'img', 'mask.jpg'}),
        path.join{'/', '2020', 'img'}
      )
    end),
    test('drops trailing path sep', function ()
      assert.are_equal(
        path.directory(path.join{'this', 'that'} .. path.separator),
        path.join{'this', 'that'}
      )
    end),
    test('returns dot if given just a filename', function ()
      assert.are_equal(
        path.directory('index.html'),
        '.'
      )
    end)
  },

  group 'filename' {
    test('removes directories', function ()
      assert.are_equal(
        path.filename(path.join{'paper', 'refs.bib'}),
        'refs.bib'
      )
      assert.are_equal(
        path.filename(path.join{'www', '.htaccess'}),
        '.htaccess'
      )
    end),
    test('empty if no filename present', function ()
      assert.are_equal(
        path.filename(path.join{'usr', 'bin'} .. path.separator),
        ''
      )
    end)
  },

  group 'is_absolute' {
    test('network path is absolute', function ()
      local paths = {'c:', 'Applications'}
      assert.is_truthy(path.is_absolute '//foo/bar')
    end),
    test('pure filename is not absolute', function ()
      assert.is_falsy(path.is_absolute '1337.txt')
    end),
  },

  group 'is_relative' {
    test('joined paths are relative', function ()
      local paths = {'one', 'two', 'test'}
      assert.is_truthy(path.is_relative(path.join(paths)))
    end),
    test('pure filename is relative', function ()
      assert.is_truthy(path.is_relative '1337.txt')
    end),
    test('network path is not relative', function ()
      assert.is_falsy(path.is_relative '//foo/bar')
    end),
  },

  group 'splitting/joining' {
    test('split_path is inverse of join', function ()
      local paths = {'one', 'two', 'test'}
      assert.are_same(path.split(path.join(paths)), paths)
    end),
  },

  group 'split_extensions' {
    test('filename', function ()
      assert.are_equal(path.split_extension 'image.jpg', 'image')
    end),
    test('extension', function ()
      assert.is_truthy(select(2, path.split_extension 'thesis.tex'), '.tex')
      assert.are_equal(select(2, path.split_extension 'fstab'), '')
    end),
    test('concat gives inverts split', function ()
      local filenames = {'/etc/passwd', '34a90-1.bat', 'backup.tar.gz'}
      for _, filename in ipairs(filenames) do
        local base, ext = path.split_extension(filename)
        assert.are_equal(base .. ext, filename)
      end
    end),
  },

  group 'normalize' {
    test('removes leading `./`', function ()
      assert.are_equal(path.normalize('./a.md'), 'a.md')
    end),
    test('dedupe path separators', function ()
      assert.are_equal(path.normalize('a//b'), path.join{'a', 'b'})
    end)
  },

  group 'relative or absolute' {
    test('xor', function ()
      local test_paths = {
        path.join{ 'hello', 'rudi'},
        path.join{ '.', 'autoexec.bat'},
        path.join{ 'C:', 'config.sys'},
        path.join{ '/', 'etc', 'passwd'}
      }
      for _, fp in ipairs(test_paths) do
        assert.is_truthy(path.is_relative(fp) == not path.is_absolute(fp))
      end
    end)
  },

  group 'strings as path objects' {
    test('setup', path.treat_strings_as_paths),
    test('split extension', function ()
      local img = 'mandrill.jpg'
      assert.are_equal(img:split_extension(), 'mandrill')
    end),
    test('conbine paths with `/`', function ()
      assert.are_equal('a' / 'b', path.join{'a', 'b'})
    end),
    test('add extension with `+`', function ()
      assert.are_equal('a' + 'b', path.join{'a.b'})
    end),
  },

  group 'make_relative' {
    test('just the filename if file is within path', function()
      assert.are_equal(
        path.make_relative('/foo/bar/file.txt', '/foo/bar'),
        'file.txt'
      )
    end),
    test('no change if name outside of reference dir', function()
      assert.are_equal(
        path.make_relative('/foo/baz/file.txt', '/foo/bar'),
        '/foo/baz/file.txt'
      )
    end),
    test('use `..` when allowing unsafe operation', function()
      assert.are_equal(
        path.make_relative('/foo/baz/file.txt', '/foo/bar', true),
        path.join{'..', 'baz', 'file.txt'}
      )
    end),
    test('return dot if both paths are the same', function()
      assert.are_equal(
        path.make_relative('/one/two/three', '/one/two/three/'),
        '.'
      )
    end)
  },
}
