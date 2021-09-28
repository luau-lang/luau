# Change indexing on string object to error for unknown keys

## Summary

Instead of indexing on string objects returning `nil` for keys that aren't methods on the `string` global, emit an error to surface a likely bug early.

## Motivation

string is the only type other than tables that supports indexing by default. This depends on the host configuration, but by default all versions of Lua and Luau
configure the global metatable for string type to be `{ __index = string }`, where `string` here is the global string function table, containing methods such as `format` and `sub`.

This is necessary to support method calls[^1] on string objects, such as `("%d"):format(42)`. As a byproduct of metatable lookup rules, it also means that using a key that doesn't
exist in the `string` global table results in `nil` being returned.

This is problematic, because this hides errors from people who a new to the language when they try to index a string using numbers:

```lua
local ch = str[2] -- returns nil; instead this should be str:sub(2) or str:byte(2)
```

as well as making accidental errors when traversing data structures more difficult to discover:

```lua
if player.Name and player.Name.Health > 100 then -- meant to use player.Health, now this condition is always false
...
end
```

When type checking is used, it should be able to surface both kinds of errors, but especially in non-strict mode there are often cases when type checking doesn't know the
type of an object being indexed, and can't therefore emit an error.

## Design

Right now the global metatable for string type is set to `{ __index = string }`. This means that indexing of strings is forwarded to string global, which returns nil on keys that
aren't present.

Instead, we're going to use a chain of two metatables like this (pseudocode):

```lua
MT1 = { __index = MT2 }
MT2 = { ...string fields } with metatable MT3
MT3 = { __index = noindex }
noindex = function() error("indexing is not supported on strings") end
```

As a result, all lookups will go to MT2 instead of string global, and if they use a valid method name, it's going to be found in the table, but unknown keys will trigger a
second-level metatable lookup which will call `noindex` function, producing an error.

As a result, some programs may change behavior and start generating errors when previously they produced `nil`. The premise of this proposal is that code like this likely
is incorrect to begin with, and the error is just being hidden.

Note that the pseudo-code above is a sketch that is easy to understand, but in practice there's an alternative formulations where one of the tables uses itself as a metatable:

```lua
MT1 = { __index = MT2 }
MT2 = { ... string fields, __index = noindex } with metatable MT2
```

## Drawbacks

This change technically breaks backwards compatibility, because it renders technically valid programs invalid. For example, this code works as long as the input is a table
or a string today, but will break after this change:

```
function kind(v)
  return v.kind and v.kind or tostring(v)
end
```

This change makes the core object structure slightly more complicated because instead of just a single hidden metatable on strings we get two.

This change means that if the embedder customizes the string global table, all the additions must be replicated manually in the extra hidden metatable, which complicates the setup.

## Alternatives

Besides the obvious alternative, which is "do nothing", a variant of this change that was at some point [present in the upstream code](https://github.com/lua/lua/commit/8980c630bf40e05dad71ded377e3d0f0a17b076c), but was removed before being released in any
point release, was considered: instead of adding extra metatables, `string` global could gain a `__index` key (referring to `noindex`) and set itself as its own metatable.

This change is easier mechanically since it doesn't have some drawbacks of this proposal (keeps structure easier to understand and is transparently compatible with extensions to string table),
but in addition to changing semantics on string object lookup it also changes it for lookups to string global, which means that code like this is no longer valid:

```
if string.pack then
... -- code compatible with Lua 5.3 / Luau
end
```

As such this alternative is believed to be more surprising than the proposed design.

[^1]: It's unfortunate that this feature was added to Lua decades ago, but it's so fundamental that removing it will break most programs in existence.
