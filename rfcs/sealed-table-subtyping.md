# Sealed table subtyping

**Status**: Implemented

## Summary

In Luau, tables have a state, which can, among others, be "sealed". A sealed table is one that we know the full shape of and cannot have new properties added to it. We would like to introduce subtyping for sealed tables, to allow users to express some subtyping relationships that they currently cannot.

## Motivation

We would like this code to type check:
```lua
type Interface = {
    name: string,
}

type Concrete = {
    name: string,
    id: number,
}

local x: Concrete = {
    name = "foo",
    id = 123,
}

local function getImplementation(): Interface
    return x
end
```
Right now this code fails to type check, because `x` contains an extra property, `id`. Allowing sealed tables to be subtypes of other sealed tables would permit this code to type check successfully.

## Design

In order to do this, we will make sealed tables act as a subtype of other sealed tables if they contain all the properties of the supertype.

```
type A = {
    name: string,
}

type B = {
    name: string,
    id: number,
}

type C = {
    id: number,
}

local b: B = {
    name = "foo",
    id = 123,
}

-- works: B is a subtype of A
local a: A = b

-- works: B is a subtype of C
local c: C = b

-- fails: A is not a subtype of C
local a2: A = c
```

This change affects existing code, but it should be a strictly more permissive change - it won't break any existing code, but it will allow code that was previously denied before.

## Drawbacks

This change will mean that sealed tables that don't exactly match may be permitted. In the past, this was an error; users may be relying on the type checker to perform these checks. We think the risk of this is minimal, as the presence of extra properties is unlikely to break user code. This is an example of code that would have raised a type error before:

```lua
type A = {
    name: string,
}

local a: A = {
    name = "foo",
    -- Before, we would have raised a type error here for the presence of the
    -- extra property `id`.
    id = 123,
}
```

## Alternatives

In order to avoid any chance of breaking backwards-compatibility, we could introduce a new state for tables, "interface" or something similar, that can only be produced via new syntax. This state would act like a sealed table, except with the addition of the subtyping rule described in this RFC. An example syntax for this:

```lua
-- `interface` context-sensitive keyword denotes an interface table
type A = interface {
    name: string,
}

type B = {
    name: string,
    id: number,
}

local b: B = {
    name = "foo",
    id = 123,
}

local a: A = b
```
