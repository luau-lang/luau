# Improve and extend the boolean comparison metamethods

## Summary

`__eq`, `__le`, and `__lt` metamethods will return values as-is instead of coercing to boolean, and `__ne`, `__ge` and `__gt` will be introduced.

## Motivation

Lua 5.x has metamethods for comparing two non-trivial (e.g. userdata or table) types through the use of the `__eq` (equality), `__le` (less than or equal) and `__lt` (less than) metamethods.

When Lua has determined one of these metamethods should be invoked, it automatically coerces the returned value to a boolean value even if the returned value is not intended to be so.

While this is the typical usecase for boolean comparison operators, there are some cases where returning yet another table or userdata value might be appropriate - for example, when creating more complex bindings to mathematical libraries, or when creating more DSL-like APIs.

Further, Lua omits the three inverse metamethods, analogously named `__ne` (not equal), `__ge` (greater than or equal) and `__gt` (greater than). While the results of these metamethods can be inferred directly from their existing counterparts, it removes the ability for runtimes to bind specifically to those operations - especially in the case of `~=`, which loses information (the negation) when invoking the `__eq` metamethod.

One use case for this extended metamethod behavior is the use of linear constraint solvers to be used by user interface components in custom game engines. It is often the case that constraints are built up as a system of equations, for example `x = Var(); y = Var(); solver:add(x == y + 14)`. Here, the addition operator would trigger an `__add` metamethod invocation due to `y` being a userdata/table value, and then `__eq` would be invoked due to Luau's new functionality. However, since Lua force-coerces the results of boolean comparison operators to a boolean, `:add()` would receive either `true` or `false` instead of some constraint object. In the past this has been worked around using terrible hacks for logging the order of operations of a constraint and checking if the last known operation was a boolean equality check, at which point an internally cached constraint object would be added to the solver system in the case `:add()` was passed a boolean. Naturally this is a terrible solution, as it requires the constraint is added to the solver _directly_ after the `==` operation at the risk of causing unexpected or adverse behavior.

Further, while `>=` and `>` can both be reversed to `<=` and `<` (and thus adding them in this RFC might be considered excessive or unnecessary), the use of `~=` (Lua's 'not equal' operator) cannot, as there is no way for the runtime to capture that the `__eq` method is being used within a negated expression (i.e. a 'not equal' (`~=`) expression).

Thus, since the three aforementioned operators would have their semantics slightly adjusted, and the `__ne` operator would be introduced, this was motivation to include the `__ge` and `__gt` metamethods for completeness.

## Design

Two potential designs involve a performance/compatibility tradeoff.

In the pro-performance case, Luau would introduce all of the above functionality and _remove_ the additional fallback logic for `__le` (whereby `a <= b` falls back to `not (b < a)`), assumedly reducing the number of branches in the general case. This would require any existing implementations to define both `__lt` **and** `__le` in order for the `<=` to continue working.

This can be trivially patched using the following Lua function (note: untested):

```lua
function patch_le(obj)
	if type(obj) ~= 'table' then return end
	local mt = getmetatable(obj)
	if not mt.__lt then return end
	mt.__le = function(a, b) return not (b < a) end
end
```

In the pro-compatibility case, Luau would instead keep this support, but would also need to support the fallbacks for `__ge` -> `__gt`, `__ne` -> `__eq`, and potentially even the cross-comparisons between `__gt` -> `__lte` and `__gte` -> `__lt` (and potentially further, the inverse of those).

This would potentially introduce considerable performance overhead in the form of extra branching.

In either case, the force boolean coersion would be removed entirely, which could mean a performance improvement at the cost of breaking edge cases where an existing metamethod returns a non-boolean value, relying on the boolean coersion behavior, and then having its return value explicitly compared with boolean (either in a conditional or in a place where strict types are checked). The workaround would be to wrap an expression `x` which _must_ be boolean-typed with `(not not x)`, as there is no `toboolean()` function in standard Lua (and one does not exist in Luau, to my knowledge).

It is worth noting that `x or y` where `x` is boolean-coerced to `true` will yield `x`, _not_ `true`, so there _is_ a precedent where Lua boolean comparison operators return non-boolean typed values.

## Drawbacks

To some, this may seem like a more complicated means of operator overloading than the status-quo. The introduction of more metamethods means more runtime overhead, more places to look for functionality, and backwards incompatibility with existing scripts.

## Alternatives

Without changing semantics (even in a backwards compatible manner), the only alternative is to use `:gt()`, `:add()`, `:eq()`, `:ne()`, etc. type methods for such custom bindings, as one might expect.

However, it might be worth exploring the idea of passing an _additional_ parameter to the _existing_ metamethods to indicate under which circumstances they are being used (e.g. `__eq` would receive `true` as the third parameter if it's being used in a `~=` expression, and `nil` / simply no parameter (depending on who's looking) in the positive-equality case, to keep things backwards compatible).

The downside to the latter alternative is that it still does not 'fix' the forced boolean coersion behavior of the boolean comparison metamethods.

Finally, to reduce the scope of work or of changes, the addition of `__ge` and `__gt` can be omitted as they are transposable to `__le` and `__lt`, albeit this debatably removes some of the syntactic expressiveness available to script authors.
