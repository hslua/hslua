# hslua-module-path

[![GitHub CI][CI badge]](https://github.com/hslua/hslua-module-paths/actions)
[![Hackage][Hackage badge]](https://hackage.haskell.org/package/hslua-module-path)
[![Stackage Lts][Stackage Lts badge]](http://stackage.org/lts/package/hslua-module-path)
[![Stackage Nightly][Stackage Nightly badge]](http://stackage.org/nightly/package/hslua-module-path)
[![MIT license][License badge]](LICENSE)

[CI badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[Hackage badge]: https://img.shields.io/hackage/v/hslua-module-path.svg?logo=haskell
[Stackage Lts badge]: http://stackage.org/package/hslua-module-path/badge/lts
[Stackage Nightly badge]: http://stackage.org/package/hslua-module-path/badge/nightly
[License badge]: https://img.shields.io/badge/license-MIT-blue.svg

Lua module to work with file paths.

## path

Module for file path manipulations.

#### separator

The character that separates directories.

#### search\_path\_separator

The character that is used to separate the entries in the `PATH`
environment variable.

### Functions

#### directory (filepath)

Get the directory name; move up one level.

Parameters:

filepath  
path (string)

Returns:

-   The filepath up to the last directory separator. (string)

#### filename (filepath)

Get the file name.

Parameters:

filepath  
path (string)

Returns:

-   File name part of the input path. (string)

#### is\_absolute (filepath)

Checks whether a path is absolute, i.e. not fixed to a root.

Parameters:

filepath  
path (string)

Returns:

-   `true` iff `filepath` is an absolute path, `false` otherwise.
    (boolean)

#### is\_relative (filepath)

Checks whether a path is relative or fixed to a root.

Parameters:

filepath  
path (string)

Returns:

-   `true` iff `filepath` is a relative path, `false` otherwise.
    (boolean)

#### join (filepaths)

Join path elements back together by the directory separator.

Parameters:

filepaths  
path components (list of strings)

Returns:

-   The joined path. (string)

#### make\_relative (path, root, unsafe)

Contract a filename, based on a relative path. Note that the resulting
path will never introduce `..` paths, as the presence of symlinks means
`../b` may not reach `a/b` if it starts from `a/c`. For a worked example
see [this blog
post](http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html).

Parameters:

path  
path to be made relative (string)

root  
root path (string)

unsafe  
whether to allow `..` in the result. (boolean)

Returns:

-   contracted filename (string)

#### normalize (filepath)

Normalizes a path.

-   `//` outside of the drive can be made blank
-   `/` becomes the `path.separator`
-   `./` -&gt; ’’
-   an empty path becomes `.`

Parameters:

filepath  
path (string)

Returns:

-   The normalized path. (string)

#### split (filepath)

Splits a path by the directory separator.

Parameters:

filepath  
path (string)

Returns:

-   List of all path components. (list of strings)

#### split\_extension (filepath)

Splits the last extension from a file path and returns the parts. The
extension, if present, includes the leading separator; if the path has
no extension, then the empty string is returned as the extension.

Parameters:

filepath  
path (string)

Returns:

-   filepath without extension (string)

-   extension or empty string (string)

#### split\_search\_path (search\_path)

Takes a string and splits it on the `search_path_separator` character.
Blank items are ignored on Windows, and converted to `.` on Posix. On
Windows path elements are stripped of quotes.

Parameters:

search\_path  
platform-specific search path (string)

Returns:

-   list of directories in search path (list of strings)
