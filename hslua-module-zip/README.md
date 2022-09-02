# hslua-module-zip

[![GitHub CI][CI badge]](https://github.com/hslua/hslua-module-zips/actions)
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

Module for file zip manipulations.

#### separator

The character that separates directories.

#### search\_zip\_separator

The character that is used to separate the entries in the `PATH`
environment variable.

### Functions

#### directory (filezip)

Get the directory name; move up one level.

Parameters:

filezip  
zip (string)

Returns:

-   The filezip up to the last directory separator. (string)

#### filename (filezip)

Get the file name.

Parameters:

filezip  
zip (string)

Returns:

-   File name part of the input zip. (string)

#### is\_absolute (filezip)

Checks whether a zip is absolute, i.e. not fixed to a root.

Parameters:

filezip  
zip (string)

Returns:

-   `true` iff `filezip` is an absolute zip, `false` otherwise.
    (boolean)

#### is\_relative (filezip)

Checks whether a zip is relative or fixed to a root.

Parameters:

filezip  
zip (string)

Returns:

-   `true` iff `filezip` is a relative zip, `false` otherwise.
    (boolean)

#### join (filezips)

Join zip elements back together by the directory separator.

Parameters:

filezips  
zip components (list of strings)

Returns:

-   The joined zip. (string)

#### make\_relative (zip, root, unsafe)

Contract a filename, based on a relative zip. Note that the resulting
zip will never introduce `..` zips, as the presence of symlinks means
`../b` may not reach `a/b` if it starts from `a/c`. For a worked example
see [this blog
post](http://neilmitchell.blogspot.co.uk/2015/10/filezips-are-subtle-symlinks-are-hard.html).

Parameters:

zip  
zip to be made relative (string)

root  
root zip (string)

unsafe  
whether to allow `..` in the result. (boolean)

Returns:

-   contracted filename (string)

#### normalize (filezip)

Normalizes a zip.

-   `//` outside of the drive can be made blank
-   `/` becomes the `zip.separator`
-   `./` -&gt; ’’
-   an empty zip becomes `.`

Parameters:

filezip  
zip (string)

Returns:

-   The normalized zip. (string)

#### split (filezip)

Splits a zip by the directory separator.

Parameters:

filezip  
zip (string)

Returns:

-   List of all zip components. (list of strings)

#### split\_extension (filezip)

Splits the last extension from a file zip and returns the parts. The
extension, if present, includes the leading separator; if the zip has
no extension, then the empty string is returned as the extension.

Parameters:

filezip  
zip (string)

Returns:

-   filezip without extension (string)

-   extension or empty string (string)

#### split\_search\_zip (search\_zip)

Takes a string and splits it on the `search_zip_separator` character.
Blank items are ignored on Windows, and converted to `.` on Posix. On
Windows zip elements are stripped of quotes.

Parameters:

search\_zip  
platform-specific search zip (string)

Returns:

-   list of directories in search zip (list of strings)
