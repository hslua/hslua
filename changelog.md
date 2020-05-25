# hslua-aeson

## v1.0.2

Released 2020-05-25

- Relaxed version constraint for aeson, allowing `aeson-1.5.*`.

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
