# Unsealed table assignment creates an optional property

**Status**: Implemented

## Summary

In Luau, tables have a state, which can, among others, be "unsealed".
An unsealed table is one that we are still constructing.  Currently
assigning a table literal to an unsealed table does not introduce new
properties, so it is a type error if they are read.
We would like to change this so that assigning a table
literal to an unsealed table creates an optional property.

## Motivation

In lua-apps, there is testing code which (simplified) looks like:

```lua
local t = { u = {} }
t = { u = { p = 37 } }
t = { u = { q = "hi" } }
local x: number? = t.u.p
local y: string? = t.u.q
```

Currently, this code doesn't typecheck, due to `p` and `q` being unknown properties of `t.u`.

## Design

In order to support this idiom, we propose that assigning a table
to an unsealed table should add an optional property.

For example, before this change the type of `t` is `{ u: {} }`,
and after this change is `{ u: { p: number?, q: number? } }`.

This is implemented by adding a case to unification where the supertype
is an unsealed table, and the subtype is a table with extra properties.
Currently the extra properties are ignored, but with this change we would
add the property to the unsealed table (making it optional if necessary).

Since tables with optional properties of the same type are subtypes of
tables with indexers, this allows table literals to be used as dictionaries,
for example the type of `t` is a subtype of `{ u: { [string]: number } }`.

Note that we need to add an optional property, otherwise the example above will not typecheck.
```lua
local t = { u = {} }
t = { u = { p = 37 } }
t = { u = { q = "hi" } } -- fails because there's no u.p
```

## Drawbacks

The implementation of this proposal introduces optional types during unification,
and so needs access to an allocator.

## Alternatives

Rather than introducing optional properties, we could introduce an indexer. For example we could infer the type of 
`t` as `{ u: { [string]: number } }`.
