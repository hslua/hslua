# Changelog

`hslua-objectorientation` uses [PVP Versioning][1].

## hslua-objectorientation 2.0.1

Release 2021-11-04.

  - Excludes absent properties from `pairs`: Properties that are
    optional and not present in a sum-type value are no longer
    included in the iterator output produced by `pairs` (i.e., the
    `__pairs` metamethod). Previously, the names of absent
    properties were pushed with a `nil` value.

## hslua-objectorientation 2.0.0

Release 2021-10-21.

- Published without warning.

[1]: https://pvp.haskell.org
