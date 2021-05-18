# Always call `__eq` when comparing for equality

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

`__eq` metamethod will always be called during `==`/`~=` comparison, even for objects that are rawequal.

## Motivation

Lua 5.x has the following algorithm it uses for comparing userdatas and tables:

- If two objects are not of the same type (userdata vs number), they aren't equal
- If two objects are referentially equal, they are equal (!)
- If no object has a metatable with `__eq` metamethod, they are equal iff they are referentially equal
- Otherwise, pick one of the `__eq` metamethods, call it with both objects as arguments and return the result.

In mid-2019, we've released Luau which implements a fast path for userdata comparison. This fast path accidentally omitted step 2 for userdatas with C `__eq` implementations (!), and thus comparing a userdata object vs itself would actually run `__eq` metamethod. This is significant as it allowed users to use `v == v` as a NaN check for vectors, coordinate frames, and other objects that have floating point contents.

Since this was a bug, we're in a rather inconsistent state:

- `==` and `~=` in the code always call `__eq` for userdata with C `__eq`
- `==` and `~=` don't call `__eq` for tables and custom newproxy-like userdatas with Lua `__eq` when objects are ref. equal
- `table.find` *doesn't* call `__eq` when objects are ref. equal

## Design

Since developers started relying on `==` behavior for NaN checks in the last two years since Luau release, the bug has become a feature. Additionally, it's sort of a good feature since it allows to implement NaN semantics for custom types - userdatas, tables, etc.

Thus the proposal suggests changing the rules so that when `__eq` metamethod is present, `__eq` is always called even when comparing the object to itself.

This would effectively make the current ruleset for userdata objects official, and change the behavior for `table.find` (which is probably not significant) and, more significantly, start calling user-provided `__eq` even when the object is the same. It's expected that any reasonable `__eq` implementation can handle comparing the object to itself so this is not expected to result in breakage.

## Drawbacks

This represents a difference in a rather core behavior from all upstream versions of Lua.

## Alternatives

We could instead equalize (ha!) the behavior between Luau and Lua. In fact, this is what we tried to do initially as the userdata behavior was considered a bug, but encountered the issue with games already depending on the new behavior.

We could work with developers to change their games to stop relying on this. However, this is more complicated to deploy and - upon reflection - makes `==` less intuitive than the main proposal when comparing objects with NaN, since e.g. it means that these two functions have a different behavior:

```
function compare1(a: Vector3, b: Vector3)
    return a == b
end

function compare2(a: Vector3, b: Vector3)
    return a.X == b.X and a.Y == b.Y and a.Z == b.Z
end
```

## References

https://devforum.roblox.com/t/call-eq-even-when-tables-are-rawequal/1088886
https://devforum.roblox.com/t/nan-vector3-comparison-broken-cframe-too/1130778
