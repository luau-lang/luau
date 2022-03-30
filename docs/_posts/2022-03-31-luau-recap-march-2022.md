---
layout: single
title:  "Luau Recap: March 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-march-2022/).]

## Singleton types

We added support for singleton types! These allow you to use string or
boolean literals in types. These types are only inhabited by the
literal, for example if a variable `x` has type `"foo"`, then `x ==
"foo"` is guaranteed to be true.

Singleton types are particularly useful when combined with union types,
for example:

```lua
type Animals = "Dog" | "Cat" | "Bird"
```

or:

```lua
type Falsey = false | nil
```

In particular, singleton types play well with unions of tables,
allowing tagged unions (also known as discriminated unions):

```lua
type Ok<T> = { type: "ok", value: T }
type Err<E> = { type: "error", error: E }
type Result<T, E> = Ok<T> | Err<E>

local result: Result<number, string> = ...
if result.type == "ok" then
    -- result :: Ok<number>
    print(result.value)
else
    -- result :: Err<string>
    error(result.error)
end
```

The RFC for singleton types is https://github.com/Roblox/luau/blob/master/rfcs/syntax-singleton-types.md

## Width subtyping

A common idiom for programming with tables is to provide a public interface type, but to keep some of the concrete implementation private, for example:

```lua
type Interface = {
    name: string,
}

type Concrete = {
    name: string,
    id: number,
}
```

Within a module, a developer might use the concrete type, but export functions using the interface type:

```lua
local x: Concrete = {
    name = "foo",
    id = 123,
}

local function get(): Interface
    return x
end
```

Previously examples like this did not typecheck but now they do!

This language feature is called *width subtyping* (it allows tables to get *wider*, that is to have more properties).

The RFC for width subtyping is https://github.com/Roblox/luau/blob/master/rfcs/sealed-table-subtyping.md

## Typechecking improvements

 * Generic function type inference now works the same for generic types and generic type packs.
 * We improved some error messages.
 * There are now fewer crashes (hopefully none!) due to mutating types inside the Luau typechecker.
 * We fixed a bug that could cause two incompatible copies of the same class to be created.
 * Luau now copes better with cyclic metatable types (it gives a type error rather than hanging).
 * Fixed a case where types are not properly bound to all of the subtype when the subtype is a union.
 * We fixed a bug that confused union and intersection types of table properties.
 * Functions declared as `function f(x : any)` can now be called as `f()` without a type error.

## API improvements

 * Implement `table.clone` which takes a table and returns a new table that has the same keys/values/metatable. The cloning is shallow - if some keys refer to tables that need to be cloned, that can be done manually by modifying the resulting table.

## Debugger improvements

 * Use the property name as the name of methods in the debugger.

## Performance improvements

 * Optimize table rehashing (~15% faster dictionary table resize on average)
 * Improve performance of freeing tables (~5% lift on some GC benchmarks)
 * Improve gathering performance metrics for GC.
 * Reduce stack memory reallocation.

