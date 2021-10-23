# hslua-aeson

## v2.0.0

Release pending.

- Changed module name from `Foreign.Lua.Aeson` to `HsLua.Aeson`.

## v1.0.3.1

Released 2020-10-16.

- Allow hslua-1.3.*.

## v1.0.3

Released 2020-08-15.

- Relaxed version constraint for hslua, allowing `hslua-1.2.*`.

## v1.0.2

Released 2020-05-28

- Relaxed version constraint for aeson, allowing `aeson-1.5.*`.

- Update CI tests to check with GHC versions 8.0 through 8.10.
  Compilation with GHC 7.10 is no longer tested.

- Bump to stackage LTS-14.

## v1.0.1

Released 2020-04-03

- Relax version constraint for packages hashable and hslua, allow
  `hashable-1.3` and `hslua-1.1.*`.

## v1.0.0

- Update to hslua 1.0.0

- Function `registerNull` has been replaced by `pushNull`.

  Using `pushNull` has the advantage that users won't have to remember
  to register a special variable. Users who need a global variable can
  set it by running

        pushNull
        setglobal "HSLUA_AESON_NULL"


## v0.3.0

- Update to hslua 0.8.0.


## v0.2.0

- Update to hslua 0.6.0.


## v0.1.0.4

- Ensure compatibility with hslua 0.5.0.
