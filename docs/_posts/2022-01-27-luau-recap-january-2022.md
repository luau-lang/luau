---
layout: single
title:  "Luau Recap: January 2022"
---

Luau is our programming language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Find us on GitHub](https://github.com/Roblox/luau)!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-january-2022/).]

## Performance improvements

The implementation of `tostring` has been rewritten.  This change replaces the default number->string conversion with a
new algorithm called Schubfach, which allows us to produce the shortest precise round-trippable representation of any
input number very quickly.

While performance is not the main driving factor, this also happens to be significantly faster than our old
implementation (up to 10x depending on the number and the platform).

---

Make `tonumber(x)` ~2x faster by avoiding reparsing string arguments.

---

The Luau compiler now optimizes table literals where keys are constant variables the same way as if they were constants, eg

```lua
local r, g, b = 1, 2, 3
local col = { [r] = 255, [g] = 0, [b] = 255 }
```

## Improvements to type assertions

The `::` type assertion operator can now be used to coerce a value between any two related types.  Previously, it could
only be used for downcasts or casts to `any`.  The following used to be invalid, but is now valid:

```lua
local t = {x=0, y=0}
local a = t :: {x: number}
```

## Typechecking improvements

An issue surrounding table literals and indexers has been fixed:

```lua
type RecolorMap = {[string]: RecolorMap | Color3}

local hatRecolorMap: RecolorMap = {
    Brim = Color3.fromRGB(255, 0, 0), -- We used to report an error here
    Top = Color3.fromRGB(255, 0, 0)
}
```

---
Accessing a property whose base expression was previously refined will now return the correct result.

## Linter improvements

`table.create(N, {})` will now produce a static analysis warning since the element is going to be shared for all table entries.

## Error reporting improvements

When a type error involves a union (or an option), we now provide more context in the error message.

For instance, given the following code:

```lua
--!strict

type T = {x: number}

local x: T? = {w=4}
```

We now report the following:

```
Type 'x' could not be converted into 'T?'
caused by:
  None of the union options are compatible. For example: Table type 'x' not compatible with type 'T' because the former is missing field 'x'
```

---
Luau now gives up and reports an `*unknown*` type in far fewer cases when typechecking programs that have type errors.

## New APIs

We have brought in the [`coroutine.close`](https://luau-lang.org/library#coroutine-library) function from Lua 5.4.  It accepts a suspended coroutine and marks it as non-runnable. In Roblox, this can be useful in combination with `task.defer` to implement cancellation.

## REPL improvements

The `luau` REPL application can be compiled from source or downloaded from [releases page](https://github.com/Roblox/luau/releases).  It has grown some new features:

* Added `--interactive` option to run the REPL after running the last script file.
* Allowed the compiler optimization level to be specified.
* Allowed methods to be tab completed
* Allowed methods on string instances to be completed
* Improved Luau REPL argument parsing and error reporting
* Input history is now saved/loaded

## Thanks

A special thanks to all the fine folks who contributed PRs over the last few months!

* [Halalaluyafail3](https://github.com/Halalaluyafail3)
* [JohnnyMorganz](https://github.com/JohnnyMorganz)
* [Kampfkarren](https://github.com/Kampfkarren)
* [kunitoki](https://github.com/kunitoki)
* [MathematicalDessert](https://github.com/MathematicalDessert)
* [metatablecat](https://github.com/metatablecat)
* [petrihakkinen](https://github.com/petrihakkinen)
* [rafa_br34](https://github.com/rafa_br34)
* [Rerumu](https://github.com/Rerumu)
* [Slappy826](https://github.com/Slappy826)
* [SnowyShiro](https://github.com/SnowyShiro)
* [vladmarica](https://github.com/vladmarica)
* [xgladius](https://github.com/xgladius)

[Contribution guide](https://github.com/Roblox/luau/blob/2f989fc049772f36de1a4281834c375858507bda/CONTRIBUTING.md)
