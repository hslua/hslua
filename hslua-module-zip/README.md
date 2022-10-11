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

## Functions

### toarchive (binary archive string)

Reads an *Archive* structure from a raw zip archive; throws an error
if the given string cannot be decoded into an archive.

*Since: 1.0.0*

Parameters:

binary archive string
:    (string)

Returns:

 -   (ZipArchive)

### create (entries_or_filepaths, opts)

Creates a new archive. If a list of ZipEntry objects is given,
then a new archive with just these entries is created. For a list
of file paths, this function reads these files and adds them to
the repository.


*Since: 1.0.0*

Parameters:

entries_or_filepaths
:    ({string,...}|{ZipEntry,...})

opts
:   zip options (table)

Returns:

 -  a new archive (ZipArchive)

### read_entry (filepath, opts)

Generates a ZipEntry from a file or directory.


*Since: 1.0.0*

Parameters:

filepath
:    (string)

opts
:   zipping options (table)

Returns:

 -  a new zip archive entry (ZipEntry)
