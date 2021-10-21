# hslua-packaging

[![Build status][GitHub Actions badge]][GitHub Actions]
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Hackage]](https://hackage.haskell.org/package/hslua-packaging)

Utilities to package up Haskell functions and values into a Lua
module.

[GitHub Actions badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[GitHub Actions]: https://github.com/hslua/hslua/actions
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/main?svg=true
[Hackage]: https://img.shields.io/hackage/v/hslua-packaging.svg

This package is part of [HsLua], a Haskell framework built around
the embeddable scripting language [Lua].

[HsLua]: https://hslua.org/
[Lua]: https://lua.org/

## Functions

It is rarely enough to just expose Haskell functions to Lua, they
must also be documented. This library allows to combine both into
one step, as one would do in source files.

Functions can be exposed to Lua if they follow the type

    a_0 -> a_1 -> ... -> a_n -> LuaE e b

where each a~i~, 0 ≤ i ≤ n can be retrieved from the Lua stack.

Let's look at an example: we want to expose the *factorial*
function, making use of Haskell's arbitrary size integers. Below
is how we would document and expose it to Lua.

``` haskell
-- | Calculate the factorial of a number.
factorial :: DocumentedFunction Lua.Exception
factorial = defun "factorial"
  ### liftPure (\n -> product [1..n])
  <#> n
  =#> productOfNumbers
  #? "Calculates the factorial of a positive integer."
  `since` makeVersion [1,0,0]
 where
   n :: Parameter Lua.Exception Integer
   n = parameter peekIntegral "integer"
         "n"
         "number for which the factorial is computed"

   productOfNumbers :: FunctionResults Lua.Exception Integer
   productOfNumbers =
     functionResult pushIntegral "integer"
       "produce of all numbers from 1 upto n"
```

This produces a value which can be pushed to Lua as a function

``` haskell
pushDocumentedFunction factorial
setglobal "factorial"
```

and can then be called from Lua

``` lua
> factorial(4)
24
> factorial(23)
"25852016738884976640000"
```

The documentation can be rendered as Markdown with `renderFunction`:

```
factorial (n)

Calculates the factorial of a positive integer.

*Since: 1.0.0*

Parameters:

n
:   number for which the factorial is computed (integer)

Returns:

 - product of all integers from 1 upto n (integer)
```
