---
layout: single
title:  "Luau Recap: April 2021"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau). Another busy month in Luau with many performance improvements.

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-april-2021/).]

## Editor features

Luau implementation now provides an internal API for type-aware autocomplete suggestions.

Roblox Studio will be the first user of this API and we plan for a new beta feature to come soon in addition to existing Luau-powered beta features like Go To Declaration, Type Hovers and Script Function Filter (you should check those out!)

## Performance improvements

Performance is a very important part of Luau implementation and we continue bringing in new performance optimizations:

* We've finished the work on internal `vector` value type that will be used by `Vector3` type in Roblox. Improvements of up to 10x can be seen for primitive operations and some of our heavy `Vector3` benchmarks have seen 2-3x improvement. You can read more about this feature [on Roblox Developer forums](https://devforum.roblox.com/t/native-luau-vector3-beta/)
* By optimizing the way string buffers are handled internally, we bring improvements to string operations including `string.lower`, `string.upper`, `string.reverse`, `string.rep`, `table.concat` and string concatenation operator `..`. Biggest improvements can be seen on large strings
* Improved performance of `table.insert` and `table.remove`. Operations in the middle of large arrays can be multiple times faster with this change
* Improved performance of internal table resize which brings additional 30% speedup for `table.insert`
* Improved performance of checks for missing table fields

## Generic functions

We had to temporarily disable generic function definitions last month after finding critical issues in the implementation.

While they are still not available, we are making steady progress on fixing those issues and making additional typechecking improvements to bring them back in.

## Debugger improvements

Debugging is now supported for parallel Luau Actors in Roblox Studio.

Read more about the feature [on Roblox Developer forums](https://devforum.roblox.com/t/parallel-lua-beta/) and try it out yourself.

## Behavior changes

Backwards compatibility is important for Luau, but sometimes a change is required to fix corner cases in the language / libraries or to improve performance. Even still, we try to keep impact of these changes to a minimum:

* __eq tag method will always get called for table comparisons even when a table is compared to itself

## Coming soon...

* Better type refinements for statements under a condition using a new constraint resolver. Luau will now understand complex conditions combining `and`/`not` and type guards with more improvements to come
