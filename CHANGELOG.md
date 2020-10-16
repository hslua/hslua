# Revision history for tasty-lua

## 0.2.3.1 -- 2020-10-16

- Relaxed upper bound for hslua, allow `hslua-1.3.*`.

## 0.2.3 -- 2020-08-14

- CI now also builds with for GHC 8.10.

- Errors are now explicitly converted to strings before matched
  when using `error_matches`.

- Relax version limits for tasty and hslua, allowing tasty-1.3.*
  and hslua-1.2.*.

## 0.2.2 -- 2020-01-26

- Avoid compilation warnings on GHC 8.2 and older. Monoid
  instances on older GHC versions require an explicit
  implementation of `mappend`. Newer instances use `(<>)` from
  Semigroup.

- Improved CI tests: build with more GHC versions, build with
  stack, and ensure that there are no HLint errors.

## 0.2.1 -- 2020-01-26

- Fixed an issue with error reporting: the bug caused test-group
  names to be added multiple times when reporting a test failure.

## 0.2.0.1 -- 2019-06-19

- List all files in cabal file: *stack.yaml* and
  *test/tasty-lua.lua* were added to the list of extra source
  files.

## 0.2.0 -- 2019-05-19

- Renamed `testFileWith` to `testLuaFile`, and
  `testsFromFile` to `translateResultsFromFile`.

- Fixed and extended test summary: if all tests pass, a brief
  summary about the number of passed tests is show. Furthermore,
  some bugs (caused by a misused Foldable instance) have been
  fixed.

- Code has been split into multiple sub-modules.

## 0.1.1 -- 2019-05-17

- Add new function `testFileWith`, allowing to run a file as a
  single test case. Lua tests should be defined with `tasty.lua`.
  Failures, if any, are summarized in the failure message of the
  test.

## 0.1.0 -- 2019-05-11

* First version. Released on an unsuspecting world.
