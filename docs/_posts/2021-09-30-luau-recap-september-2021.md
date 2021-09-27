---
layout: single
title:  "Luau Recap: September 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-september-2021/).]

## Editor features

We have made some improvements to the Luau-powered autocomplete beta feature in Roblox Studio:

 * We no longer give autocomplete suggestions for client-only APIs in server-side scripts,
   or vice versa.
 * For table literals with known shape, we provide autocomplete suggestions for properties.
 * We provide autocomplete suggestions for `Player.PlayerGui`.
 * Keywords such as `then` and `else` are autocompleted better.
 * Autocompletion is disabled inside a comment span (a comment starting `--[[`).

## Typechecking improvements

The big news this month is that generic functions are back!

*write-up of generics goes here*

In other typechecking news:

 * The Luau constraint resolver can now refine the operands of equality expressions.
 * We fixed some crashes caused by UAF during type inference.
 * We do a better job of tracking updates when script is moved inside the data model.
 * We fixed one of the ways that [recursive types could cause free types to leak](https://devforum.roblox.com/t/free-types-leaked-into-this-modules-public-interface/1459070).
 * We improved the way that `return` statements interact with mutually recursive
   function declarations.
 * We improved parser recovery from code which looks like a function call (but isn't) e.g.
```
local x = y
(expr)[smth] = z
```
 * We consistently report parse errors before type errors.
 * We display more types as `*unknown*` rather than as an internal type name like `error####`.
 * Luau now infers the result of `Instance:Clone()` much more accurately.
 * Luau type guard refinements now support more arbitrary cases, for instance `typeof(foo) ~= "Instance"` eliminates anything not a subclass of Instance.


## Performance improvements

 * We optimized the performance of the `Vector3.new` constructor.

## Other improvements

 * We fixed some GC issues where objects could become "zombies".
 * We relaxed the Parallel Lua constraints when dispatching deferred events.
