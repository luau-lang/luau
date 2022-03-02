---
layout: single
title:  "Luau Recap: February 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-february-2022/).]

## Default type alias type parameters

We have introduced a syntax to provide default type arguments inside the type alias type parameter list.

It is now possible to have type functions where the instantiation can omit some type arguments.

You can provide concrete types:

```lua
--!strict
type FieldResolver<T, Data = {[string]: any}> = (T, Data) -> number

local a: FieldResolver<number> = ...
local b: FieldResolver<number, {name: string}> = ...
```

Or reference parameters defined earlier in the list:

```lua
--!strict
type EqComp<T, U = T> = (l: T, r: U) -> boolean

local a: EqComp<number> = ... -- (l: number, r: number) -> boolean
local b: EqComp<number, string> = ... -- (l: number, r: string) -> boolean
```

Type pack parameters can also have a default type pack:

```lua
--!strict
type Process<T, U... = ...string> = (T) -> U...

local a: Process<number> = ... -- (number) -> ...string
local b: Process<number, (boolean, string)> = ... -- (number) -> (boolean, string)
```

If all type parameters have a default type, it is now possible to reference that without providing any type arguments:

```lua
--!strict
type All<T = string, U = number> = (T) -> U

local a: All -- ok
local b: All<> -- ok as well
```

For more details, you can read the original [RFC proposal](https://github.com/Roblox/luau/blob/master/rfcs/syntax-default-type-alias-type-parameters.md).

## Typechecking improvements

This month we had many fixes to improve our type inference and reduce false positive errors.

if-then-else expression can now have different types in each branch:

```lua
--!strict
local a = if x then 5 else nil -- 'a' will have type 'number?'
local b = if x then 1 else '2' -- 'b' will have type 'number | string'
```

And if the expected result type is known, you will not get an error in cases like these:

```lua
--!strict
type T = {number | string}
-- different array element types don't give an error if that is expected
local c: T = if x then {1, "x", 2, "y"} else {0}
```

---

`assert` result is now known to not be 'falsy' (`false` or `nil`):

```lua
--!strict
local function f(x: number?): number
    return assert(x) -- no longer an error
end
```

---

We fixed cases where length operator `#` reported an error when used on a compatible type:

```lua
--!strict
local union: {number} | {string}
local a = #union -- no longer an error
```

---

Functions with different variadic argument/return types are no longer compatible:

```lua
--!strict
local function f(): (number, ...string)
    return 2, "a", "b"
end

local g: () -> (number, ...boolean) = f -- error
```

---

We have also fixed:

* false positive errors caused by incorrect reuse of generic types across different function declarations
* issues with forward-declared intersection types
* wrong return type annotation for table.move
* various crashes reported by developers

## Linter improvements

A new static analysis warning was introduced to mark incorrect use of a '`a and b or c`' pattern. When 'b' is 'falsy' (`false` or `nil`), result will always be 'c', even if the expression 'a' was true:

```lua
local function f(x: number)
    -- The and-or expression always evaluates to the second alternative because the first alternative is false; consider using if-then-else expression instead
    return x < 0.5 and false or 42
end
```

Like we say in the warning, new if-then-else expression doesn't have this pitfall:

```lua
local function g(x: number)
    return if x < 0.5 then false else 42
end
```

---

We have also introduced a check for misspelled comment directives:

```lua
--!non-strict
-- ^ Unknown comment directive 'non-strict'; did you mean 'nonstrict'?
```

## Performance improvements

For performance, we have changed how our Garbage Collector collects unreachable memory.  
This rework makes it possible to free memory 2.5x faster and also comes with a small change to how we store Luau objects in memory. For example, each table now uses 16 fewer bytes on 64-bit platforms.

Another optimization was made for `select(_, ...)` call.  
It is now using a special fast path that has constant-time complexity in number of arguments (~3x faster with 10 arguments).

## Thanks

A special thanks to all the fine folks who contributed PRs this month!

* [mikejsavage](https://github.com/mikejsavage)
* [TheGreatSageEqualToHeaven](https://github.com/TheGreatSageEqualToHeaven)
* [petrihakkinen](https://github.com/petrihakkinen)
