---
layout: single
title:  "Luau Recap: November 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-november-2021/).]

## Type packs in type aliases

Type packs are the construct Luau uses to represent a sequence of types. We've had syntax for generic type packs for a while now, and it sees use in generic functions, but it hasn't been available in type aliases. That has changed, and it is now syntactically legal to write the following type alias:
```lua
type X<A...> = () -> A...
type Y = X<number, string>
```

We've also added support for explicit type packs. Previously, it was impossible to instantiate a generic with two or more type pack parameters, because it wasn't clear where the first pack ended and the second one began. We have introduced a new syntax for this use case:
```
type Fn<P..., R...> = (P...) -> R...
type X = Fn<(number, string), (string, number)>
```

For more information, check out [the documentation](https://luau-lang.org/typecheck#type-packs) or [the RFC](https://github.com/Roblox/luau/blob/f86d4c6995418e489a55be0100159009492778ff/rfcs/syntax-type-alias-type-packs.md) for this feature.

## Luau is open-source!

We announced this in early November but it deserves repeating: Luau is now an open-source project! You can use Luau outside of Roblox, subject to MIT License, and - importantly - we accept contributions.

Many changes contributed by community, both Roblox and external, have been merged since we've made Luau open source. Of note are two visible changes that shipped on Roblox platform:

- The type error "Expected to return X values, but Y values are returned here" actually had X and Y swapped! This is now fixed.
- Luau compiler dutifully computed the length of the string when using `#` operator on a string literal; this is now fixed and `#"foo"` compiles to 3.

You might think that C++ is a scary language and you can't contribute to Luau. If so, you'd be happy to know that the contents of https://luau-lang.org, where we host our documentation, is also hosted on GitHub in the same repository (https://github.com/Roblox/luau/tree/master/docs) and that we'd love the community to contribute improvements to documentation among other changes! For example see [issues in this list](https://github.com/Roblox/luau/issues?q=is%3Aissue+is%3Aopen+label%3A%22pr+welcome%22) that start with "Documentation", but all other changes and additions to documentation are also welcome.

## Library improvements

```lua
function bit32.countlz(n: number): number
function bit32.countrz(n: number): number
```
Given a number, returns the number of preceding left or trailing right-hand bits that are `0`.

See [the RFC for these functions](https://github.com/Roblox/luau/blob/f86d4c6995418e489a55be0100159009492778ff/rfcs/function-bit32-countlz-countrz.md) for more information.

## Type checking improvements

We have enabled a rewrite of how Luau handles `require` tracing. This has two main effects: firstly, in strict mode, `require` statements that Luau can't resolve will trigger type errors; secondly, Luau now understands the `FindFirstAncestor` method in `require` expressions.

Luau now warns when the index to `table.move` is 0, as this is non-idiomatic and performs poorly. If this behavior is intentional, wrap the index in parentheses to suppress the warning.

Luau now provides additional context in table and class type mismatch errors.

## Performance improvements

We have enabled several changes that aim to avoid allocating a new closure object in cases where it's not necessary to. This is helpful in cases where many closures are being allocated; in our benchmark suite, the two benchmarks that allocate a large number of closures improved by 15% and 5%, respectively.

When checking union types, we now try possibilities whose synthetic names match. This will speed up type checking unions in cases where synthetic names are populated.

We have also enabled an optimization that shares state in a hot path on the type checker. This will improve type checking performance.

The Luau VM now attempts to cache the length of tables' array portion. This change showed a small performance improvement in benchmarks, and should speed up `#` expressions.

The Luau type checker now caches a specific category of table unification results. This can improve type checking performance significantly when the same set of types is used frequently.

When Luau is not retaining type graphs, the type checker now discards more of a module's type surface after type checking it. This improves memory usage significantly.

## Bug fixes

We've fixed a bug where on ARM systems (mobile), packing negative numbers using unsigned formats in `string.pack` would produce the wrong result.

We've fixed an issue with type aliases that reuse generic type names that caused them to be instantiated incorrectly.

We've corrected a subtle bug that could cause free types to leak into a table type when a free table is bound to that table.

We've fixed an issue that could cause Luau to report an infinitely recursive type error when the type was not infinitely recursive.
