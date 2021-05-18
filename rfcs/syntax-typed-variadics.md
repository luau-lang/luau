# Typed variadics

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add syntax for ascribing a type to variadic pack (`...`).

## Motivation

Luau's type checker internally can represent a typed variadic: any number of values of the same type. Developers should be able to describe this construct in their own code, for cases where they have a function that accepts an arbitrary number of `string`s, for example.

## Design

We think that the postfix `...: T` syntax is the best balance of readability and simplicity. In function type annotations, we will use `...T`:

```
function math.max(...: number): number
end

type fn = (...string) -> string

type fn2 = () -> ...string
```

This doesn't introduce syntactical ambiguity and should cover all cases where we need to represent this construct. Like `...` itself, this syntax is only legal as the last parameter to a function.

Like all type annotations, the `...: T` syntax has no effect on runtime behavior versus an unannotated `...`.

There are currently no plans to introduce named variadics, but this proposal leaves room to adopt them with the form `...name: Type` in function declarations in the future.

## Drawbacks

The mismatch between the type of `...` in function declaration (`number`) and type declaration (`...number`) is a bit awkward. This also gets more complicated when we introduce generic variadic packs.

## Alternatives

We considered several other syntaxes for this construct:

* `...T`: leaves no room to introduce named variadics
* `...: T...`: redundant `...`
* `... : ...T`: feels redundant, same as above
* `...: T*`: potentially confusing for users with C knowledge, where `T*` is a pointer type
