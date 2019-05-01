# Revision history for hslua-module-system

## 0.2.0 -- 2019-05-01

All fields and functions are now exported from the Haskell module under
the same name as that used in Lua.

### New fields

- `arch`: processor architecture.
- `compiler_name`: Haskell compiler that was used to compile the module.
- `compiler_version`: version of the compiler.
- `os`: operating system.

### New functions

- `mkdir`: create a new directory.
- `rmdir`: remove a directory.
- `with_env`: perform action with custom environment.
- `with_wd`: perform action in another directory.

### Removed or renamed functions

- `currentdir` was renamed to `getwd`.
- `chdir` was renamed to `setwd`.
- `pwd` was removed.

### Misc

- Fix typos and copy-paste errors in docs, tests.

## 0.1.0 -- 2019-04-26

- First version. Released on an unsuspecting world.
