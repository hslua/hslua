# hslua-module-zip

[![GitHub CI][CI badge]](https://github.com/hslua/hslua/actions)
[![Hackage][Hackage badge]](https://hackage.haskell.org/package/hslua-module-zip)
[![Stackage Lts][Stackage Lts badge]](http://stackage.org/lts/package/hslua-module-zip)
[![Stackage Nightly][Stackage Nightly badge]](http://stackage.org/nightly/package/hslua-module-zip)
[![MIT license][License badge]](LICENSE)

[CI badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[Hackage badge]: https://img.shields.io/hackage/v/hslua-module-zip.svg?logo=haskell
[Stackage Lts badge]: http://stackage.org/package/hslua-module-zip/badge/lts
[Stackage Nightly badge]: http://stackage.org/package/hslua-module-zip/badge/nightly
[License badge]: https://img.shields.io/badge/license-MIT-blue.svg

Lua module to work with file zips.

## zip

Functions to create, modify, and extract files from zip archives.

The module can be called as a function, in which case it behaves
like the `zip` function described below.

Zip options are optional; when defined, they must be a table with
any of the following keys:

  - `recursive`: recurse directories when set to `true`;
  - `verbose`: print info messages to stdout;
  - `destination`: the value specifies the directory in which to extract;
  - `location`: value is used as path name, defining where files
    are placed.
  - `preserve_symlinks`: Boolean value, controlling whether
    symbolic links are preserved as such. This option is ignored
    on Windows.

## Functions

### Archive

`Archive (bytestring_or_entries)`

Reads an *Archive* structure from a raw zip archive or a list of
Entry items; throws an error if the given string cannot be decoded
into an archive.

*Since: 1.0.0*

Parameters:

bytestring_or_entries
:    (string|{ZipEntry,...})

Returns:

 -   (ZipArchive)

### Entry

`Entry (path, contents[, modtime])`

Generates a zip Entry from a filepath, the file's uncompressed
content, and the file's modification time.

*Since: 1.0.0*

Parameters:

path
:   file path in archive (string)

contents
:   uncompressed contents (string)

modtime
:   modification time (integer)

### read_entry

`read_entry (filepath, opts)`

Generates a ZipEntry from a file or directory.

*Since: 1.0.0*

Parameters:

filepath
:    (string)

opts
:   zip options (table)

Returns:

 -  a new zip archive entry (ZipEntry)

### zip

`zip (filepaths[, options])`

Package and compress the given files into a new Archive.

*Since: 1.0.0*

Parameters:

filepaths
:    list of files from which the archive is created. ({string,...})

options
:   zip options (table)

Returns:

 -  a new archive (ZipArchive)

## Types

### Archive

A zip archive with file entries.

#### Fields

`entries`
:   files in this zip archive ({Entry,...})

#### Methods

`extract()`
:   Extract all files from this archive, creating directories as
    needed. Note that the last-modified time is set correctly only
    in POSIX, not in Windows. This function fails if encrypted
    entries are present.

`bytestring()`
:   Returns the raw binary string representation of the archive.

### Entry

File or directory entry in a zip archive.

#### Fields:

`path`
:   relative path, using `/` as separator

`modtime`
:   modification time (seconds since unix epoch)

#### Methods:

`contents([password])`
:   Get the uncompressed contents of a zip entry. If `password` is
    given, then that password is used to decrypt the contents. An
    error is throws if decrypting fails.
