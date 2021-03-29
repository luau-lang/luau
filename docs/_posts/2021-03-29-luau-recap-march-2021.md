---
layout: single
title:  "Luau Recap: March 2021"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau). It's been a busy month in Luau!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-march-2021/).]

## Typed variadics

[TODO]

## Generic functions

[TODO]

## Typechecking improvements

* Check bodies of methods whose `self` has type `any`
* More precise types for `debug.*` methods
* Better typechecking for `Instance:IsA`.

## Performance improvements

* Optimized `coroutine.resume`
  (a 10% speed-up in benchmarks)
* New implementation of string mutations, which mutates in place rather than copying if there's
  only one copy of the string buffer (a 20% speedup)
* New `FASTCALL` specializations, optimizing common cases for methods such as `table.insert`
  (a 40% speedup)
* Improved `table.move` performance (often 3x faster!)

## Library changes

* Added the `debug.info` function which allows retrieving information about stack frames or functions; similarly to `debug.getinfo` from Lua, this accepts an options string that must consist of characters `slnfa`; unlike Lua that returns a table, the function returns all requested values one after another to improve performance.

## New logo

Luau now has a shiny new logo!

!["New logo!"]({{ site.url }}{{ site.baseurl }}/assets/images/luau.png)

## Coming soon...

* Better treatment of cyclic requires
* [Anything else?]