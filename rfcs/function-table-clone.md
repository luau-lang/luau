# table.clone

**Status**: Implemented

## Summary

Add `table.clone` function that, given a table, produces a copy of that table with the same keys/values/metatable.

## Motivation

There are multiple cases today when cloning tables is a useful operation.

- When working with tables as data containers, some algorithms may require modifying the table that can't be done in place for some reason.
- When working with tables as objects, it can be useful to obtain an identical copy of the object for further modification, preserving the metatable.
- When working with immutable data structures, any modification needs to clone some parts of the data structure to produce a new version of the object.

While it's possible to implement this function in user code today, it's impossible to implement it with maximum efficiency; furthermore, cloning is a reasonably fundamental
operation so from the ergonomics perspective it can be expected to be provided by the standard library.

## Design

`table.clone(t)` takes a table, `t`, and returns a new table that:

- has the same metatable
- has the same keys and values
- is not frozen, even if `t` was

The copy is shallow: implementing a deep recursive copy automatically is challenging (for similar reasons why we decided to avoid this in `table.freeze`), and often only certain keys need to be cloned recursively which can be done after the initial clone.

The table can be modified after cloning; as such, functions that compute a slightly modified copy of the table can be easily built on top of `table.clone`.

`table.clone(t)` is functionally equivalent to the following code, but it's more ergonomic (on the account of being built-in) and significantly faster:

```lua
assert(type(t) == "table")
local nt = {}
for k,v in pairs(t) do
  nt[k] = v
end
if type(getmetatable(t)) == "table" then
  setmetatable(nt, getmetatable(t))
end
```

The reason why `table.clone` can be dramatically more efficient is that it can directly copy the internal structure, preserving capacity and exact key order, and is thus
limited purely by memory bandwidth. In comparison, the code above can't predict the table size ahead of time, has to recreate the internal table structure one key at a time,
and bears the interpreter overhead (which can be avoided for numeric keys with `table.move` but that doesn't work for the general case of dictionaries).

Out of the abundance of caution, `table.clone` will fail to clone the table if it has a protected metatable. This is motivated by the fact that you can't do this today, so
there are no new potential vectors to escape various sandboxes. Superficially it seems like it's probably reasonable to allow cloning tables with protected metatables, but
there may be cases where code manufactures tables with unique protected metatables expecting 1-1 relationship and cloning would break that, so for now this RFC proposes a more
conservative route. We are likely to relax this restriction in the future.

## Drawbacks

Adding a new function to `table` library theoretically increases complexity. In practice though, we already effectively implement `table.clone` internally for some VM optimizations, so exposing this to the users bears no cost.

Assigning a type to this function is a little difficult if we want to enforce the "argument must be a table" constraint. It's likely that we'll need to type this as `table.clone(T): T` for the time being, which is less precise.

## Alternatives

We can implement something similar to `Object.assign` from JavaScript instead, that simultaneously assigns extra keys. However, this won't be fundamentally more efficient than
assigning the keys afterwards, and can be implemented in user space. Additionally, we can later extend `clone` with an extra argument if we so choose, so this proposal is the
minimal viable one.

We can immediately remove the rule wrt protected metatables, as it's not clear that it's actually problematic to be able to clone tables with protected metatables.
