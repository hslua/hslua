---
layout: default
author: Albert Krewinkel
title: Santa's Little Lua Scripts
date: 2020-12-06
categories: example
---

# Santa's Little Lua Scripts

Santa sighted deeply as worry and uncertainty gave way, leaving a
feeling of relieve and accomplishment. The year was one of the
worst he'd seen so far. Large numbers of his helpers were moving
from the North Pole to Antarctica to satisfy their ambient
temperature preferences. There would be many telecommuting Elves
this year, and each helper enjoyed additional autonomy. Tying
everything together was a challenge. But he had succeeded: the
wishes processing program was finished, and the elves would be
able to help Santa from the comfort of their new homes.

## Wishes

The part of the wishes system that Santa had been working on was
focused on classic toys: wooden bricks, dolls, and train sets.

``` haskell
data Toy = Bricks | TrainSet | Doll deriving Show
```

The system also kept track of basic data about the children:

``` haskell
data Behavior = Nice | Naughty deriving (Eq, Show)

data Child = Child
  { childName     :: Text
  , childBehavior :: Behavior
  } deriving (Show)
```

Children and toys were tied together in a wish.

``` haskell
data Wish = Wish
  { wishingChild :: Child
  , wishedToy    :: Toy
  } deriving (Show)
```

It was most elegant. The problem for Santa was that the Elves,
being independent and autonomous workers, needed to access and
process the data in very custom ways. Unfortunately for him, very
few Elves had a Haskell build environment installed, so he had to
distribute the binary. Writing a completely custom processing
language seemed like an enormous rabbit hole.

## Lua

Fortunately, Santa had a better idea: [Lua], an embeddable
scripting language. He had been using it for some projects[^1]
and also made use of it in [pandoc], which he used to answer his
mails. Santa would just need to expose the relevant parts of the
Haskell system, so the Elves could access and script it as their
hearts desired. He looked for a library, found [HsLua], and got
to work.

[^1]: Santa learned about Lua from his game-devs. Now he uses it
    to keep his security teams on their toes with [nmap] and
    [Wireshark]; many of the North Pole's servers contain custom
    Lua scripts, too ([redis], [nginx/OpenResty], [HAProxy],
    [PowerDNS]).

[nmap]: https://nmap.org
[Wireshark]: https://wireshark.org/
[redis]: https://redis.io/commands/eval
[nginx/OpenResty]: https://github.com/openresty
[HAProxy]: https://www.haproxy.com/blog/5-ways-to-extend-haproxy-with-lua/
[PowerDNS]: https://doc.powerdns.com/authoritative/lua-records/index.html

## Exposing data

Lua has a simple, yet powerful, stack-based [API]. The first step
towards exposing Haskell data was to push them to the Lua stack.
Keeping things simple, Santa chose strings to represent toys:

``` haskell
pushToy :: Toy -> Lua ()
pushToy = pushString . show
```

Lua offers only a single construct to structure data: tables. So
that's what Child and Wish were represented with.

``` haskell
pushChild :: Child -> Lua ()
pushChild (Child name behavior) = do
  -- create new Lua table on the stack
  newtable
  -- push string to stack
  pushText name
  -- table now in position 2; assign string to field in table
  setfield (nth 2) "name"

  -- push boolean to stack
  pushBool (behavior == Nice)
  setfield (nth 2) "nice"

pushWish :: Wish -> Lua ()
pushWish (Wish child toy) = do
  newtable
  pushChild child
  setfield (nth 2) "child"
  pushToy toy
  setfield (nth 2) "toy"
```

## Running scripts

Santa's goal for now was to allow his Elves to filter the list of
wishes so each finds the ones relevant to them. For example, if
an Elf only cares about wishes for train sets from children who
were nice, then they should be able to use a script to filter
those wishes out.

``` lua
return function (wish)
  return wish.child.nice and
    wish.toy == 'TrainSet'
end
```

The script returns a (lambda) function that serves as a predicate
for wishes. The function can be thought of having the type `Wish
-> IO Bool`. Santa needed to turn the Lua lambda function into an
actual Haskell function `runPredicate :: Wish -> Lua Bool`. If
Santa assumed that the lambda function was at the top of the Lua
stack, then he could push a `Wish` value to the Lua stack, call
the function, and retrieve the result value from the stack.

``` haskell
runPredicate :: Wish -> Lua Bool
runPredicate wish = do
  -- Assume filter function is at the top of the stack;
  -- create a copy so we can re-use it.
  pushvalue top
  pushWish wish
  -- Call the function. There is one argument on the stack,
  -- and we expect one result to be returned.
  call (NumArgs 1) (NumResults 1)
  toboolean top <* pop 1
```

What remained was loading the Elves' script files. Santa did this
with [`dofile`] of type `FilePath -> Lua Status`. The predicate
then ends up on the top of the Lua stack, and can be called
through `runPredicate`, e.g. to select a subset of wishes via
[`filterM`].

``` haskell
main :: IO ()
main = do
  filterFile <- fmap (!! 0) getArgs -- get first argument
  result <- run $ do
    _status <- dofile filterFile
    filterM runPredicate wishes
  print result
```

Santa tested his creation on a short list of wishes

``` haskell
wishes :: [Wish]
wishes =
  [ Wish (Child "Theodor" Nice) Bricks
  , Wish (Child "Philine" Nice) TrainSet
  , Wish (Child "Steve" Naughty) Doll
  ]
```

by running `runhaskell wish-filter predicate.lua`. To his
uttermost satisfaction, the terminal echoed the right information
back to him.

    [Wish {wishingChild = Child {childName = "Philine", childBehavior = Nice}, wishedToy = TrainSet}]

He reclined in his chair, shut down his device, and enjoyed a
double chocolate chip cookie of which he felt very deserving now.

[Lua]: https://lua.org/
[HsLua]: https://github.com/hslua/hslua
[pandoc]: https://pandoc.org/lua-filters.html
[API]: https://www.lua.org/manual/5.3/manual.html#4
[`dofile`]: https://hackage.haskell.org/package/hslua/docs/Foreign-Lua-Core.html#v:dofile
[`filterM`]: https://hackage.haskell.org/package/base/docs/Control-Monad.html#v:filterM

-----------------------------------------------------------------

Santa's full code, as presented here, is available as part of the
examples at <https://github.com/hslua/hslua>.

-----------------------------------------------------------------

[Modernes Publizieren]: https://oa-pub.hos.tuhh.de/de/oa-advent-cal/

<style>
hr {
  margin: 4em 2em;
}
</style>
