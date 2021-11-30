# Revision history for hslua-module-system

## 1.0.1

Release pending.

- Relaxed upper bound of hslua-packaging allowing version 2.1.

## 1.0.0

Release pending.

- Use hslua 2.0.

## 0.2.2.1 -- 2020-10-16

- Relaxed upper bound for hslua, allow `hslua-1.3.*`.

## 0.2.2 -- 2020-08-15

- Relaxed upper bound for hslua, allow `hslua-1.2.*`.
- Improved documentation of internal types.
- Use tasty-lua for unit tests.
- Update CI to test with all GHC versions.

## 0.2.1 -- 2019-05-04

- Use module helpers made available with HsLua 1.0.3. This avoids
  code duplication when used with other hslua modules.

## 0.2.0 -- 2019-05-01

All fields and functions are now exported from the Haskell module
under the same name as that used in Lua.

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
