---
layout: single
title:  "Luau Recap: March 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-march-2022/).]

## Tagged unions

## Width subtyping

## Typechecking improvements

 * Generic function type inference now works the same for generic types and generic type packs.
 * Less crashing due to mutating types which shouldn't be.
 * Fix a bug that could cause two incompatible copies of the same class to be created.
 * Cope better with cyclic metatable types.
 * Add support for `table.clone`.
 * Better error messages.
 * Fix a bug that confused union and intersection types of table properties.

## Debugger improvements

 * Use the property name as the name of methods in the debugger.

## Performance improvements

 * Improve gathering performance metrics for GC.
 * Reduce stack memory reallocation.

## Thanks

A special thanks to all the fine folks who contributed PRs this month!

* [ProBaturay](https://github.com/ProBaturay)
