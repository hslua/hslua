hslua-module-text
=================

[![GitHub CI][CI badge]](https://github.com/hslua/hslua/actions)
[![Hackage][Hackage badge]](https://hackage.haskell.org/package/hslua-module-text)
[![Stackage Lts][Stackage Lts badge]](http://stackage.org/lts/package/hslua-module-text)
[![Stackage Nightly][Stackage Nightly badge]](http://stackage.org/nightly/package/hslua-module-text)
[![MIT license][License badge]](LICENSE)

[CI badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[Hackage badge]: https://img.shields.io/hackage/v/hslua-module-text.svg?logo=haskell
[Stackage Lts badge]: http://stackage.org/package/hslua-module-text/badge/lts
[Stackage Nightly badge]: http://stackage.org/package/hslua-module-text/badge/nightly
[License badge]: https://img.shields.io/badge/license-MIT-blue.svg

An UTF-8 aware subset of Lua's `string` module. The functions
provided by this module are `upper`, `lower`, `len`, `reverse`,
`sub`, and `toencoding`.

Intended usage for this package is to preload it by adding the
loader function to `package.preload`. Note that the Lua `package`
library must have already been loaded before the loader can be
added.


Example
-------

``` haskell
loadProg :: Lua Status
loadProg = do
  openlibs
  preloadModule documenteModule
  dostring $ "text = require 'text'\n"
          <> "print(text.upper 'hello')"
```


License
-------

This package is licensed under the MIT license. See
[`LICENSE`](LICENSE) for details.
