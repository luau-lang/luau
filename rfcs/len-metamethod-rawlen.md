# Support `__len` metamethod for tables and `rawlen` function

## Summary

`__len` metamethod will be called by `#` operator on tables, matching Lua 5.2

## Motivation

Lua 5.1 invokes `__len` only on userdata objects, whereas Lua 5.2 extends this to tables. In addition to making `__len` metamethod more uniform and making Luau
more compatible with later versions of Lua, this has the important advantage which is that it makes it possible to implement an index based container.

Before `__iter` and `__len` it was possible to implement a custom container using `__index`/`__newindex`, but to iterate through the container a custom function was
necessary, because Luau didn't support generalized iteration, `__pairs`/`__ipairs` from Lua 5.2, or `#` override.

With generalized iteration, a custom container can implement its own iteration behavior so as long as code uses `for k,v in obj` iteration style, the container can
be interfaced with the same way as a table. However, when the container uses integer indices, manual iteration via `#` would still not work - which is required for some
more complicated algorithms, or even to simply iterate through the container backwards.

Supporting `__len` would make it possible to implement a custom integer based container that exposes the same interface as a table does.

## Design

`#v` will call `__len` metamethod if the object is a table and the metamethod exists; the result of the metamethod will be returned if it's a number (an error will be raised otherwise).

`table.` functions that implicitly compute table length, such as `table.getn`, `table.insert`, will continue using the actual table length. This is consistent with the
general policy that Luau doesn't support metamethods in `table.` functions.

A new function, `rawlen(v)`, will be added to the standard library; given a string or a table, it will return the length of the object without calling any metamethods.
The new function has the previous behavior of `#` operator with the exception of not supporting userdata inputs, as userdata doesn't have an inherent definition of length.

## Drawbacks

`#` is an operator that is used frequently and as such an extra metatable check here may impact performance. However, `#` is usually called on tables without metatables,
and even when it is, using the existing metamethod-absence-caching approach we use for many other metamethods a test version of the change to support `__len` shows no
statistically significant difference on existing benchmark suite. This does complicate the `#` computation a little more which may affect JIT as well, but even if the
table doesn't have a metatable the process of computing `#` involves a series of condition checks and as such will likely require slow paths anyway.

This is technically changing semantics of `#` when called on tables with an existing `__len` metamethod, and as such has a potential to change behavior of an existing valid program.
That said, it's unlikely that any table would have a metatable with `__len` metamethod as outside of userdata it would not anything, and this drawback is not feasible to resolve with any alternate version of the proposal.

## Alternatives

Do not implement `__len`.
