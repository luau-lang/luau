---
layout: single
title:  "Luau Recap: March 2021"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau). It's been a busy month in Luau!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-march-2021/).]

## Typed variadics

Luau supports *variadic* functions, meaning ones which can take a variable number of arguments (varargs!) but previously there was no way to specify their type. Now you can!
```
function f(x: string, ...: number)
  print(x)
  print(...)
end
f("hi")
f("lo", 5, 27)
```
This function takes a string, plus as many numbers as you like, but if you try calling it with anything else, you'll get a type error, for example `f("oh", true)` gives an error " Type `boolean` could not be converted into `number`"

Variadics can be used in function declarations, and function types, for example
```
type T = {
  sum: (...number) -> number
}
function f(x: T)
  print(x.sum(1, 2, 3))
end
```

## Generic functions

[TODO]

## Typechecking improvements

* Check bodies of methods whose `self` has type `any`
* More precise types for `debug.*` methods
* Mutually dependent type aliases are now handled correctly

## Performance improvements

We are continuing to squeeze the performance out of all sorts of possible code; this is an ongoing process and we have many improvements in the pipeline, big and small. These are the changes that are already live:

* Significantly optimized non-variadic function calls, improving performance by up to 10% on call-heavy benchmarks
* Improve performance of `math.clamp`, `math.sign` and `math.round` by 2.3x, 2x and 1.6x respectively
* Optimized `coroutine.resume` with ~10% gains on coroutine-heavy benchmarks

Note that we work off a set of benchmarks that we consider representative of the wide gamut of code that runs on Luau. If you have code that you think should be running faster, never hesitate to open a feature request / bug report on Roblox Developer Forum!

## Library changes

* Added the `debug.info` function which allows retrieving information about stack frames or functions; similarly to `debug.getinfo` from Lua, this accepts an options string that must consist of characters `slnfa`; unlike Lua that returns a table, the function returns all requested values one after another to improve performance.

## New logo

Luau now has a shiny new logo!

!["New logo!"]({{ site.url }}{{ site.baseurl }}/assets/images/luau.png)

## Coming soon...

* Native Vector3 math with dramatic performance improvements!
* Better tools for memory analysis!
* Better treatment of cyclic requires during type checking
* Better type refinements including nil-ability checks, `and`/`or` and `IsA`
