HsLua Module: System
====================

This module provides access to system information and functionality via
Haskell's `System` module.

Intended usage for this package is to preload it by adding the loader
function to `package.preload`. Note that the Lua `package` library must
have already been loaded before the loader can be added.


Example
-------

``` haskell
loadProg :: Lua Status
loadProg = do
  openlibs
  preloadModule "system"
  -- create a temporary directory, print it's path, then delete it again.
  dostring $ "system = require 'system'\n"
          ++ "system.with_tmpdir('.', 'foo', print)"
```


Documentation
-------------

## Fields

### arch

The machine architecture on which the program is running.

### compiler_name

The Haskell implementation with which the host program was compiled.

### compiler_version

The version of `compiler_name` with which the host program was compiled.

### os

The operating system on which the program is running.

## Functions

### chdir {#system-chdir}

`chdir (directory)`

Change the working directory to the given path.

Parameters:

`directory`:
:   Path of the directory which is to become the new working directory.


### currentdir {#system-currentdir}

`currentdir ()`

Obtain the current working directory as an absolute path.

Returns:

- The current working directory (string).

### env {#system-env}

`env ()`

Retrieve the entire environment.

Returns:

- A table mapping environment variables names to their string value
  (table).

### getenv {#system-getenv}

`getenv (var)`

Return the value of the environment variable `var`, or `nil` if there
is no such value.

Parameters:

`var`:
:   name of the environment variable (string)

Returns:

- value of the variable, or nil if the variable is not defined (string
  or nil).

### ls {#system-ls}

`ls ([directory])`

List the contents of a directory.

Parameters:

`directory`:
:   Path of the directory whose contents should be listed (string).
    Defaults to `.`.

Returns:

- A table of all entries in `directory` without the special entries (`.`
  and `..`).

### pwd {#system-pwd}

`pwd ()`

An alias for [`currentdir`](#system-currentdir)

### setenv {#system-setenv}

`setenv (var, value)`

Set the specified environment variable to a new value.

Parameters:

`var`:
:   name of the environment variable (string).

`value`:
:   new value (string).

### tmpdirname {#system-tmpdirname}

`tmpdirname ()`

Returns the current directory for temporary files.

On Unix, `tmpdirname()` returns the value of the `TMPDIR` environment
variable or "/tmp" if the variable isn't defined. On Windows, the
function checks for the existence of environment variables in the
following order and uses the first path found:

- TMP environment variable.
- TEMP environment variable.
- USERPROFILE environment variable.
- The Windows directory

The operation may fail if the operating system has no notion of
temporary directory.

The function doesn't verify whether the path exists.

Returns:

- The current directory for temporary files (string).

### with\_tmpdir {#system-with_tmpdir}

`with_tmpdir ([parent_dir,] templ, callback)`

Create and use a temporary directory inside the given directory.
The directory is deleted after use.

Parameters:

`parent_dir`:
:   Parent directory to create the directory in (string). If this
    parameter is omitted, the system's canonical temporary directory is
    used.

`templ`:
:   Directory name template (string).

`callback`:
:   Function which takes the name of the temporary directory as its
    first argument (function).

Returns:

-   The result of the call to `callback`.


License
-------

This package is licensed under the MIT license. See [`LICENSE`](LICENSE)
for details.
