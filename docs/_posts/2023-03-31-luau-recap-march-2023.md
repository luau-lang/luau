---
layout: single
title:  "Luau Recap: March 2023"
---

How the time flies! The team has been busy since the last November Luau Recap working on some large updates that are coming in the future, but before those arrive, we have some improvements that you can already use!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-march-2023/).]

## Improved type refinements

Type refinements handle constraints placed on variables inside conditional blocks.

In the following example, while variable `a` is declared to have type `number?`, inside the `if` block we know that it cannot be `nil`:

```lua
local function f(a: number?)
    if a ~= nil then
        a *= 2 -- no type errors
    end
    ...
end
```

One limitation we had previously is that after a conditional block, refinements were discarded.

But there are cases where `if` is used to exit the function early, making the following code essentially act as a hidden `else` block.

We now correctly preserve such refinements and you should be able to remove `assert` function calls that were only used to get rid of false positive errors about types being `nil`.

```lua
local function f(x: string?)
    if not x then return end

    -- x is a 'string' here
end
```

Throwing calls like `error()` or `assert(false)` instead of a `return` statement are also recognized.

```lua
local function f(x: string?)
    if not x then error('first argument is nil') end

    -- x is 'string' here
end
```

Existing complex refinements like `type`/`typeof`, tagged union checks and other are expected to work as expected.

## Marking table.getn/foreach/foreachi as deprecated

`table.getn`, `table.foreach` and `table.foreachi` were deprecated in Lua 5.1 that Luau is based on, and removed in Lua 5.2.

`table.getn(x)` is equivalent to `rawlen(x)` when 'x' is a table; when 'x' is not a table, `table.getn` produces an error.

It's difficult to imagine code where `table.getn(x)` is better than either `#x` (idiomatic) or `rawlen(x)` (fully compatible replacement).

`table.getn` is also slower than both alternatives and was marked as deprecated.

`table.foreach` is equivalent to a `for .. pairs` loop; `table.foreachi` is equivalent to a `for .. ipairs` loop; both may also be replaced by generalized iteration.

Both functions are significantly slower than equivalent for loop replacements, are more restrictive because the function can't yield.

Because both functions bring no value over other library or language alternatives, they were marked deprecated as well.

You may have noticed linter warnings about places where these functions are used. For compatibility, these functions are not going to be removed.

## Autocomplete improvements

When table key type is defined to be a union of string singletons, those keys can now autocomplete in locations marked as '^':

```lua
type Direction = "north" | "south" | "east" | "west"

local a: {[Direction]: boolean} = {[^] = true}
local b: {[Direction]: boolean} = {["^"]}
local b: {[Direction]: boolean} = {^}
```

We also fixed incorrect and incomplete suggestions inside the header of `if`, `for` and `while` statements.

## Runtime improvements

On the runtime side, we added multiple optimizations.

`table.sort` is now ~4.1x faster (when not using a predicate) and ~2.1x faster when using a simple predicate.

We also have ideas on how improve the sorting performance in the future.

`math.floor`, `math.ceil` and `math.round` now use specialized processor instructions. We have measured ~7-9% speedup in math benchmarks that heavily used those functions.

A small improvement was made to builtin library function calls, getting a 1-2% improvement in code that contains a lot of fastcalls.

Finally, a fix was made to table array part resizing that brings large improvement to performance of large tables filled as an array, but at an offset (for example, starting at 10000 instead of 1).

Aside from performance, a correctness issue was fixed in multi-assignment expressions.

```lua
arr[1], n = n, n - 1
```

In this example, `n - 1` was assigned to `n` before `n` was assigned to `arr[1]`. This issue has now been fixed.

## Analysis improvements

Multiple changes were made to improve error messages and type presentation.

* Table type strings are now shown with newlines, to make them easier to read
* Fixed unions of `nil` types displaying as a single `?` character
* "Type pack A cannot be converted to B" error is not reported instead of a cryptic "Failed to unify type packs"
* Improved error message for value count mismatch in assignments like `local a, b = 2`

You may have seen error messages like `Type 'string' cannot be converted to 'string?'` even though usually it is valid to assign `local s: string? = 'hello'` because `string` is a sub-type of `string?`.

This is true in what is called Covariant use contexts, but doesn't hold in Invariant use contexts, like in the example below:

```lua
local a: { x: Model }
local b: { x: Instance } = a -- Type 'Model' could not be converted into 'Instance' in an invariant context
```

In this example, while `Model` is a sub-type of `Instance` and can be used where `Instance` is required.

The same is not true for a table field because when using table `b`, `b.x` can be assigned an `Instance` that is not a `Model`. When `b` is an alias to `a`, this assignment is not compatible with `a`'s type annotation.

---

Some other light changes to type inference include:

* `string.match` and `string.gmatch` are now defined to return optional values as match is not guaranteed at runtime
* Added an error when unrelated types are compared with `==`/`~=`
* Fixed issues where variable after `typeof(x) == 'table'` could not have been used as a table

## Thanks

A very special thanks to all of our open source contributors:

* [niansa/tuxifan](https://github.com/niansa)
* [B. Gibbons](https://github.com/bmg817)
* [Epix](https://github.com/EpixScripts)
* [Harold Cindy](https://github.com/HaroldCindy)
* [Qualadore](https://github.com/Qualadore)
