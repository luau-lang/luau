# Only strip optional properties from unsealed tables during subtyping

## Summary

Currently subtyping allows optional properties to be stripped from table types during subtyping.
This RFC proposes only allowing that when the subtype is unsealed and the supertype is sealed.

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

Currently we allow subtyping to strip away optional fields
as long as the supertype is sealed.
This is necessary for examples, for instance:
```lua
  local t : { p: number, q: string? } = { p = 5, q = "hi" }
  t = { p = 7 }
```
typechecks because `{ p : number }` is a subtype of
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

This RFC is for (2). There is a [separate RFC](unsealed-table-literals.md) for (1).

## Drawbacks

This introduces new type errors (it has to, since it is fixing a source of
unsoundness). This means that there are now false positives such as:
```lua
  local t : { p: number, q: string? } = { p = 5, q = "hi" }
  local u : { p: number } = { p = 5, q = "lo" }
  t = u
```
These false positives are so similar to sources of unsoundness
that it is difficult to see how to allow them soundly.

## Alternatives

We could just live with unsoundness.

