# Type ascriptions

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Implement syntax for type ascriptions using `::`

## Motivation

Luau would like to provide a mechanism for requiring a value to be of a specific type:

```
-- Asserts that the result of a + b is a number.
-- Emits a type error if it isn't.
local foo = (a + b) as number
```

This syntax was proposed in the original Luau syntax proposal. Unfortunately, we discovered that there is a syntactical ambiguity with `as`:

```
-- Two function calls or a type assertion?
foo() as (bar)
```

## Design

To provide this functionality without introducing syntactical confusion, we want to change this syntax to use the `::` symbol instead of `as`:

```
local foo = (a + b) :: number
```

This syntax is borrowed from Haskell, where it performs the same function.

The `::` operator will bind very tightly, like `as`:

```
-- type assertion applies to c, not (b + c).
local a = b + c :: number
```

Note that `::` can only cast a *single* value to a type - not a type pack (multiple values). This means that in the following context, `::` changes runtime behavior:

```
foo(1, bar()) -- passes all values returned by bar() to foo()
foo(1, bar() :: any) -- passes just the first value returned by bar() to foo()
```

## Drawbacks

It's somewhat unusual for Lua to use symbols as operators, with the exception of arithmetics (and `..`). Also a lot of Luau users may be familiar with TypeScript, where the equivalent concept uses `as`.

`::` may make it more difficult for us to use Turbofish (`::<>`) in the future.

## Alternatives

We considered requiring `as` to be wrapped in parentheses, and then relaxing this restriction where there's no chance of syntactical ambiguity:

```
local foo: SomeType = (fn() as SomeType)
-- Parentheses not needed: unambiguous!
bar(foo as number)
```

We decided to not go with this due to concerns about the complexity of the grammar - it requires users to internalize knowledge of our parser to know when they need to surround an `as` expression with parentheses. The rules for when you can leave the parentheses out are somewhat nonintuitive.
