# Revision history for tasty-lua

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
