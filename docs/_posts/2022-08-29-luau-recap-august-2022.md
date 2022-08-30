---
layout: single
title:  "Luau Recap: August 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-august-2022/).]

## Tables now support `__len` metamethod

See the RFC [Support `__len` metamethod for tables and `rawlen` function](https://github.com/Roblox/luau/blob/master/rfcs/len-metamethod-rawlen.md) for more details.

With generalized iteration released in May, custom containers are easier than ever to use. The only thing missing was the fact that tables didn't respect `__len`.

Simply, tables now honor the `__len` metamethod, and `rawlen` is also added with similar semantics as `rawget` and `rawset`:

```lua
local my_cool_container = setmetatable({ items = { 1, 2 } }, {
    __len = function(self) return #self.items end
})

print(#my_cool_container) --> 2
print(rawlen(my_cool_container)) --> 0
```

## `never` and `unknown` types

See the RFC [`never` and `unknown` types](https://github.com/Roblox/luau/blob/master/rfcs/never-and-unknown-types.md) for more details.

We've added two new types, `never` and `unknown`. These two types are the opposites of each other by the fact that there's no value that inhabits the type `never`, and the dual of that is every value inhabits the type `unknown`.

Type inference may infer a variable to have the type `never` if and only if the set of possible types becomes empty, for example through type refinements.

```lua
function f(x: string | number)
    if typeof(x) == "string" and typeof(x) == "number" then
        -- x: never
    end
end
```

This is useful because we still needed to ascribe a type to `x` here, but the type we used previously had unsound semantics. For example, it was possible to be able to _expand_ the domain of a variable once the user had proved it impossible. With `never`, narrowing a type from `never` yields `never`.

Conversely, `unknown` can be used to enforce a stronger contract than `any`. That is, `unknown` and `any` are similar in terms of allowing every type to inhabit them, and other than  `unknown` or `any`, `any` allows itself to inhabit into a different type, whereas `unknown` does not.

```lua
function any(): any return 5 end
function unknown(): unknown return 5 end

-- no type error, but assigns a number to x which expects string
local x: string = any()

-- has type error, unknown cannot be converted into string
local y: string = unknown()
```

To be able to do this soundly, you must apply type refinements on a variable of type `unknown`.

```lua
local u = unknown()

if typeof(u) == "string" then
    local y: string = u -- no type error
end
```

A use case of `unknown` is to enforce type safety at implementation sites for data that do not originate in code, but from over the wire.

## Argument names in type packs when instantiating a type

We had a bug in the parser which erroneously allowed argument names in type packs that didn't fold into a function type. That is, the below syntax did not generate a parse error when it should have.

```lua
Foo<(a: number, b: string)>
```

## New IntegerParsing lint

See [the announcement](https://devforum.roblox.com/t/improving-binary-and-hexadecimal-integer-literal-parsing-rules-in-luau/) for more details. We include this here for posterity.

We've introduced a new lint called IntegerParsing. Right now, it lints three classes of errors:

1. Truncation of binary literals that resolves to a value over 64 bits,
2. Truncation of hexadecimal literals that resolves to a value over 64 bits, and
3. Double hexadecimal prefix.

For 1.) and 2.), they are currently not planned to become a parse error, so action is not strictly required here.

For 3.), this will be a breaking change! See [the rollout plan](https://devforum.roblox.com/t/improving-binary-and-hexadecimal-integer-literal-parsing-rules-in-luau/#rollout-5) for details.

## New ComparisonPrecedence lint

We've also introduced a new lint called `ComparisonPrecedence`. It fires in two particular cases:

1. `not X op Y` where `op` is `==` or `~=`, or
2. `X op Y op Z` where `op` is any of the comparison or equality operators.

In languages that uses `!` to negate the boolean i.e. `!x == y` looks fine because `!x` _visually_ binds more tightly than Lua's equivalent, `not x`. Unfortunately, the precedences here are identical, that is `!x == y` is `(!x) == y` in the same way that `not x == y` is `(not x) == y`. We also apply this on other operators e.g. `x <= y == y`.

```lua
-- not X == Y is equivalent to (not X) == Y; consider using X ~= Y, or wrap one of the expressions in parentheses to silence
if not x == y then end

-- not X ~= Y is equivalent to (not X) ~= Y; consider using X == Y, or wrap one of the expressions in parentheses to silence
if not x ~= y then end

-- not X <= Y is equivalent to (not X) <= Y; wrap one of the expressions in parentheses to silence
if not x <= y then end

-- X <= Y == Z is equivalent to (X <= Y) == Z; wrap one of the expressions in parentheses to silence
if x <= y == 0 then end
```

As a special exception, this lint pass will not warn for cases like `x == not y` or `not x == not y`, which both looks intentional as it is written and interpreted.

## Function calls returning singleton types incorrectly widened

Fix a bug where widening was a little too happy to fire in the case of function calls returning singleton types or union thereof. This was an artifact of the logic that knows not to infer singleton types in cases that makes no sense to.

```lua
function f(): "abc" | "def"
    return if math.random() > 0.5 then "abc" else "def"
end

-- previously reported that 'string' could not be converted into '"abc" | "def"'
local x: "abc" | "def" = f()
```

## `string` can be a subtype of a table with a shape similar to `string`

The function `my_cool_lower` is a function `<a...>(t: t1) -> a... where t1 = {+ lower: (t1) -> a... +}`. 

```lua
function my_cool_lower(t)
    return t:lower()
end
```

Even though `t1` is a table type, we know `string` is a subtype of `t1` because `string` also has `lower` which is a subtype of `t1`'s `lower`, so this call site now type checks.

```lua
local s: string = my_cool_lower("HI")
```

## Other analysis improvements

* `string.gmatch`/`string.match`/`string.find` may now return more precise type depending on the patterns used
* Fix a bug where type arena ownership invariant could be violated, causing stability issues
* Fix a bug where internal type error could be presented to the user
* Fix a false positive with optionals & nested tables
* Fix a false positive in non-strict mode when using generalized iteration
* Improve autocomplete behavior in certain cases for `:` calls
* Fix minor inconsistencies in synthesized names for types with metatables
* Fix autocomplete not suggesting globals defined after the cursor
* Fix DeprecatedGlobal warning text in cases when the global is deprecated without a suggested alternative
* Fix an off-by-one error in type error text for incorrect use of `string.format`

## Other runtime improvements

* Comparisons with constants are now significantly faster when using clang as a compiler (10-50% gains on internal benchmarks)
* When calling non-existent methods on tables or strings, `foo:bar` now produces a more precise error message
* Improve performance for iteration of tables
* Fix a bug with negative zero in vector components when using vectors as table keys
* Compiler can now constant fold builtins under -O2, for example `string.byte("A")` is compiled to a constant
* Compiler can model the cost of builtins for the purpose of inlining/unrolling
* Local reassignment i.e. `local x = y :: T` is free iff neither `x` nor `y` is mutated/captured
* Improve `debug.traceback` performance by 1.15-1.75x depending on the platform
* Fix a corner case with table assignment semantics when key didn't exist in the table and `__newindex` was defined: we now use Lua 5.2 semantics and call `__newindex`, which results in less wasted space, support for NaN keys in `__newindex` path and correct support for frozen tables
* Reduce parser C stack consumption which fixes some stack overflow crashes on deeply nested sources
* Improve performance of `bit32.extract`/`replace` when width is implied (~3% faster chess)
* Improve performance of `bit32.extract` when field/width are constants (~10% faster base64)
* `string.format` now supports a new format specifier, `%*`, that accepts any value type and formats it using `tostring` rules

## Thanks

Thanks for all the contributions!

* [natteko](https://github.com/natteko)
* [JohnnyMorganz](https://github.com/JohnnyMorganz)
* [khvzak](https://github.com/khvzak)
* [Anaminus](https://github.com/Anaminus)
* [memery-rbx](https://github.com/memery-rbx)
* [jaykru](https://github.com/jaykru)
* [Kampfkarren](https://github.com/Kampfkarren)
* [XmiliaH](https://github.com/XmiliaH)
* [Mactavsin](https://github.com/Mactavsin)
