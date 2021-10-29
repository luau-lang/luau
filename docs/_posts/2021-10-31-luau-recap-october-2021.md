---
layout: single
title:  "Luau Recap: October 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-october-2021/).]

## if-then-else expression

In addition to supporting standard if *statements*, Luau adds support for if *expressions*.
Syntactically, `if-then-else` expressions look very similar to if statements.
However instead of conditionally executing blocks of code, if expressions conditionally evaluate expressions and return the value produced as a result.
Also, unlike if statements, if expressions do not terminate with the `end` keyword.

Here is a simple example of an `if-then-else` expression:
```lua
local maxValue = if a > b then a else b
```

`if-then-else` expressions may occur in any place a regular expression is used.
The `if-then-else` expression must match `if <expr> then <expr> else <expr>`;
it can also contain an arbitrary number of `elseif` clauses, like `if <expr> then <expr> elseif <expr> then <expr> else <expr>`.
Note that in either case, `else` is mandatory.  

Here's is an example demonstrating `elseif`:
```lua
local sign = if x < 0 then -1 elseif x > 0 then 1 else 0
```

**Note:** In Luau, the `if-then-else` expression is preferred vs the standard Lua idiom of writing `a and b or c` (which roughly simulates a ternary operator).  However, the Lua idiom may return an unexpected result if `b` evaluates to false.
The `if-then-else` expression will behave as expected in all situations.

## Library improvements

New additions to the `table` library have arrived:

```lua
function table.freeze(t)
```

Given a non-frozen table, freezes it such that all subsequent attempts to modify the table or assign its metatable raise an error.
If the input table is already frozen or has a protected metatable, the function raises an error; otherwise it returns the input table.
Note that the table is frozen in-place and is not being copied.
Additionally, only `t` is frozen, and keys/values/metatable of `t` don't change their state and need to be frozen separately if desired.

```lua
function table.isfrozen(t): boolean
```

Returns `true` if and only if the input table is frozen.

## Typechecking improvements

We continue work on our type constraint resolver and have multiple improvements this month.

We now resolve constraints that are created by `or` expressions.
In the following example, by checking against multiple type alternatives, we learn that value is a union of those types:
```lua
--!strict
local function f(x: any)
    if type(x) == "number" or type(x) == "string" then
        local foo = x -- 'foo' type is known to be 'number | string' here
        -- ...
    end
end
```

Support for `or` constraints allowed us to handle additional scenarios with `and` and `not` expressions to reduce false positives after specific type guards.

And speaking of type guards, we now correctly handle sub-class relationships in those checks:
```lua
--!strict
local function f(x: Part | Folder | string)
    if typeof(x) == "Instance" then
        local foo = x -- 'foo' type is known to be 'Part | Folder' here
    else
        local bar = x -- 'bar' type is known to be 'string' here
    end
end
```

One more fix handles the `a and b or c` expression when 'b' depends on 'a':
```lua
--!strict
function f(t: {x: number}?)
    local a = t and t.x or 5 -- 'a' is a 'number', no false positive errors here
end
```

Of course, our new if-then-else expressions handle this case as well.
```lua
--!strict
function f(t: {x: number}?)
    local a = if t then t.x else 5 -- 'a' is a 'number', no false positive errors here
end
```

---
We have extended bidirectional typechecking that was announced last month to propagate types in additional statements and expressions.
```lua
--!strict
function getSortFunction(): (number, number) -> boolean
    return function(a, b) return a > b end -- a and b are now known to be 'number' here
end

local comp = getSortFunction()

comp = function(a, b) return a < b end -- a and b are now known to be 'number' here as well
```

---
We've also improved some of our messages with union types and optional types (unions types with `nil`).

When optional types are used incorrectly, you get better messages. For example:
```lua
--!strict
function f(a: {number}?)
    return a[1] -- "Value of type '{number}?' could be nil" instead of "'{number}?' is not a table'
end
```

When a property of a union type is accessed, but is missing from some of the options, we will report which options are not valid:
```lua
--!strict
type A = { x: number, y: number }
type B = { x: number }
local a: A | B
local b = a.y -- Key 'y' is missing from 'B' in the type 'A | B'
```

---
When we enabled generic functions last month, some users might have seen a strange error about generic functions not being compatible with regular ones.

This was caused by undefined behaviour of recursive types.
We have now added a restriction on how generic type parameters can be used in recursive types: [RFC: Recursive type restriction](https://github.com/Roblox/luau/blob/master/rfcs/recursive-type-restriction.md)

## Performance improvements

An improvement to the Stop-The-World (atomic in Lua terms) stage of the garbage collector was made to reduce time taken by that step by 4x factor.
While this step only happens once during a GC cycle, it cannot be split into small parts and long times were visible as frame time spikes.

Table construction and resize was optimized further; as a result, many instances of table construction see 10-20% improvements
for smaller tables on all platforms and 20%+ improvements on Windows.

Bytecode compiler has been optimized for giant table literals, resulting in 3x higher compilation throughput for certain files on AMD Zen architecture.

Coroutine resumption has been optimized and is now ~10% faster for coroutine-heavy code.

Array reads and writes are also now a bit faster resulting in 1-3% lift in array-heavy benchmarks.
