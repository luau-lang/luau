# Unsealed table literals

## Summary

Currently the only way to create an unsealed table is as an empty table literal `{}`.
This RFC proposes making all table literals unsealed.

## Motivation

Table types can be *sealed* or *unsealed*. These are different in that:

* Unsealed table types are *precise*: if a table has unsealed type `{ p: number, q: string }`
  then it is guaranteed to have only properties `p` and `q`.

* Sealed tables support *width subtyping*: if a table has sealed type `{ p: number }`
  then it is guaranteed to have at least property `p`, so we allow `{ p: number, q: string }`
  to be treated as a subtype of `{ p: number }`

* Unsealed tables can have properties added to them: if `t` has unsealed type
  `{ p: number }` then after the assignment `t.q = "hi"`, `t`'s type is updated to be
  `{ p: number, q: string }`.  

* Unsealed tables are subtypes of sealed tables.

Currently the only way to create an unsealed table is using an empty table literal, so
```lua
  local t = {}
  t.p = 5
  t.q = "hi"
```
typechecks, but
```lua
  local t = { p = 5 }
  t.q = "hi"
```
does not.

This causes problems in examples, for instance
```lua
  local t : { p: number, q: string? } = { p = 5, q = "hi" }
  t = { p = 7 }
```
typechecks because we allow subtyping to strip away optional
properties, so `{ p : number }` is a subtype of
`{ p : number, q : string? }`. Unfortunately this is not sound,
since sealed tables support width subtyping:
```lua
  local t : { p: number, q: string? } = { p = 5, q = "hi" }
  local u : { p: number } = { p = 5, q = false }
  t = u
```

## Design

The fix for this source of unsoundness is twofold:

1. make all table literals unsealed, and
2. only allow stripping optional properties from when the
   supertype is sealed and the subtype is unsealed.

This RFC is for (1). There is a [separate RFC](unsealed-table-subtyping-strips-optional-properties.md) for (2).

## Drawbacks

Making all table literals unsealed is a conservative change, it only removes type errors.

It does encourage developers to add new properties to tables during initialization, which
may be considered poor style.

## Alternatives

We could introduce a new table state for unsealed-but-precise
tables. The trade-off is that that would be more precise, at the cost
of adding user-visible complexity to the type system.
