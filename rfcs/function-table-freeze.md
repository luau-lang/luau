# table.freeze

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add `table.freeze` which allows to make a table read-only in a shallow way.

## Motivation

Lua tables by default are freely modifiable in every possible way: you can add new fields, change values for existing fields, or set or unset the metatable.

Today it is possible to customize the behavior for *adding* new fields by setting a metatable that overrides `__newindex` (including setting `__newindex` to a function that always errors to prohibit additions of new fields).

Today it is also possible to customize the behavior of setmetatable by "locking" the metatable - this can be achieved by setting a meta-index `__metatable` to something, which would block setmetatable from functioning and force metatable to return the provided value. With this it's possible to prohibit customizations of a table's behavior, but existing fields can still be assigned to.

To make an existing table read-only, one needs to combine these mechanisms, by creating a new table with a locked metatable, which has an `__index` function pointing to the old table. However, this results in iteration and length operator not working on the resulting table, and carries a performance cost - both for creating the table, and for repeated property access.

## Design

This proposal proposes formalizing the notion of "read-only" tables by providing two new table functions:

- `table.freeze(t)`: given a non-frozen table t, freezes it; fails when t is not a table or is already frozen. Returns t.
- `table.isfrozen(t)`: given a table t, returns a boolean indicating the frozen status; fails when t is not a table.

When a table is frozen, the following is true:

- Attempts to modify the existing keys of the table fail (regardless of how they are performed - via table assignments, rawset, or any other methods like table.sort)
- Attempts to add new keys to the table fail, unless `__newindex` is defined on the metatable (in which case the assignment is routed through `__newindex` as usual)
- Attempts to change the metatable of the table fail
- Reading the table fields or iterating through the table proceeds as usual

This feature is useful for two reasons:

a) It allows an easier way to expose sandboxed objects that aren't possible to monkey-patch for security reasons. We actually already have support for freezing and use it internally on various builtin tables like `math`, we just don't expose it to Lua.

b) It allows an easier way to expose immutable objects for consistency/correctness reasons. For example, Cryo library provides an implementation of immutable data structures; with this functionality, it's possible to implement a lighter-weight library by, for example, extending a table with methods to return mutated versions of the table, but retaining the usual table interface

To limit the use of `table.freeze` to cases when table contents can be freely manipulated, `table.freeze` shall fail when the table has a locked metatable (but will succeed if the metatable isn't locked).

## Drawbacks

Exposing the internal "readonly" feature may have an impact on interoperability between scripts - for example, it becomes possible to freeze some tables that scripts may be expecting to have write access to from other scripts. Since we don't provide a way to unfreeze tables and freezing a table with a locked metatable fails, in theory the impact should not be any worse than allowing to change a metatable, but the full extents are unclear.

There may be existing code in the VM that allows changing frozen tables in ways that are benign to the current sandboxing code, but expose a "gap" in the implementation that becomes significant with this feature; thus we would need to audit all table writes when implementing this.

## Alternatives

We've considered exposing a recursive freeze. The correct generic implementation is challenging since it requires supporting infinitely nested tables when working on the C stack (or a stackless implementation that requires heap allocation); also, to handle self-recursive tables requires a separate temporary tracking table since stopping the traversal at frozen sub-tables is insufficient as their children may not have been frozen. As such, we leave recursive implementation to user code.

We've considered exposing thawing. The problem with this is that freezing is required for sandboxing, and as such we'd need to support "permafrozen" status that is separate from "frozen". This complicates implementation and we didn't find compelling use cases for thawing - if it becomes necessary we can always expose it separately.

We've considered calling this "locking", but the term has connotations coming from multithreading that aren't applicable here, and in absence of unlocking, "locking" makes a bit less sense.
