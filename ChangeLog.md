# Revision history for hslua-module-text

## 0.2.1 -- 2019-05-04

- Require at least HsLua v1.0.3: that version has better support
  for modules.

- Rename `pushModuleText` to `pushModule`. The old name is keeped
  as an alias for now.

## 0.2.0 -- 2018-09-24

- Use hslua 1.0.


## 0.1.2.2  -- 2018-03-09

- Relax upper bound for base.


## 0.1.2.1  -- 2017-11-24

- Add missing test file in the sources archive. This oversight had
  caused some stackage test failures.


## 0.1.2  -- 2017-11-17

- Run tests with Travis CI.
- Fix problems with GHC 7.8


## 0.1.1  -- 2017-11-16

- Lift restriction on base to allow GHC 7.8.


## 0.1  -- 2017-11-15

- First version. Released on an unsuspecting world.
