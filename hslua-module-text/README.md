HsLua Module: Text
==================

An UTF-8 aware subset of Lua's `string` module. The functions provided by this
module are `upper`, `lower`, `len`, `reverse`, and `sub`.

Intended usage for this package is to preload it by adding the loader function
to `package.preload`. Note that the Lua `package` library must have already been
loaded before the loader can be added.


Example
-------

``` haskell
loadProg :: Lua Status
loadProg = do
  openlibs
  preloadTextModule "text"
  dostring $ "text = require 'text'\n"
          ++ "print(text.upper 'hello')"
```


License
-------

This package is licensed under the MIT license. See [`LICENSE`](LICENSE) for
details.
