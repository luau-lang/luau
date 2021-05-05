# Singleton types

> Note: this RFC was adapted from an internal proposal that predates RFC process

## Summary

Introduce a new kind of type variable, called singleton types. They are just like normal types but has the capability to represent a constant value at runtime.

## Motivation

There are two primary drivers to add two kinds of singleton types: `string` and `boolean`.

### `string` singleton types

Luau type checker can get by mostly fine without constant string types, but it can shine at its best in user code.

One popular pattern are the abstract data types, which could be supported:

```
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

### `boolean` singleton types

At the moment, Luau type checker is completely unable to discern the state of a boolean whatsoever, which makes it impossible to determine all the possible types of the expression from any variations of `a and b`, `a and b or c`, or `a or b`.

## Design

Both design components of singleton types should be intuitive for everyone by default.

### Syntax

A constant string token as well as a constant boolean token is now allowed to show up in type annotation context.

```
type Animals = "Dog" | "Cat" | "Bird"
type TrueOrNil = true?
```

### Semantics

You are allowed to provide a constant value to the generic primitive type.

```lua
local foo: "Hello world" = "Hello world"
local bar: string = foo -- allowed

local foo: true = true
local bar: boolean = foo -- also allowed
```

The inverse is not true, because you're trying to narrow any values to a specific value.

```lua
local foo: string = "Hello world"
local bar: "Hello world" = foo -- not allowed

local foo: boolean = true
local bar: true = foo -- not allowed
```

Additionally, we can change the type binding `boolean` to actually alias to `true | false`, aka a union of two constant options, either `true` or `false`.

## Drawbacks

If done naively, type checking performance could regress severely. Especially in situations where we may need to check if the providing union of constant strings is a subset of another union of constant strings that is receiving it. The time complexity of that situation is `O(m * n)`. This could be made much worse because the time complexity of `std::string::operator==` is `O(n)` making it `O(m * n * o)` which is completely unacceptable. Fortunately, the strings are meant to be constant, so we just have to hash the string exactly once and then use this hashed value to compare for the rest of type analysis, so `O(m * n * 1)` is still `O(m * n)`. None of this is an issue with booleans and numbers (not part of this proposal) because their comparisons are always `O(1)`.
