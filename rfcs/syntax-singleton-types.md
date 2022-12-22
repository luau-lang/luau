# Singleton types

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Introduce a new kind of type variable, called singleton types. They are just like normal types but has the capability to represent a constant runtime value as a type.

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

Adding constant strings as type means that it is now legal to write
`{["foo"]:T}` as a table type. This should be parsed as a property,
not an indexer. For example:
```lua
  type T = {
    ["foo"]: number,
    ["$$bar"]: string,
    baz: boolean,
  }
```
The table type `T` is a table with three properties and no indexer.

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

## Drawbacks

This may increase the cost of type checking - since some types now need to carry a string literal value, it may need to be copied and compared. The cost can be mitigated through interning although this is not very trivial due to cross-module type checking and the need to be able to typecheck a module graph incrementally.

This may make the type system a bit more complex to understand, as many programmers have a mental model of types that doesn't include being able to use literal values as a type, and having that be a subtype of a more general value type.
