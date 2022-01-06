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

This causes problems in examples, in particular developers
may initialize properties but not methods:
```lua
  local t = { p = 5 }
  function t.f() return t.p end
```

## Design

The proposed change is straightforward: make all table literals unsealed.

## Drawbacks

Making all table literals unsealed is a conservative change, it only removes type errors.

It does encourage developers to add new properties to tables during initialization, which
may be considered poor style.

It does mean that some spelling mistakes will not be caught, for example
```lua
local t = {x = 1, y = 2}
if foo then
  t.z = 3 -- is z a typo or intentional 2-vs-3 choice?
end
```

In particular, we no longer warn about adding properties to array-like tables.
```lua
local a = {1,2,3}
a.p = 5
```

## Alternatives

We could introduce a new table state for unsealed-but-precise
tables. The trade-off is that that would be more precise, at the cost
of adding user-visible complexity to the type system.

We could continue to treat array-like tables as sealed.