# Write-only properties

## Summary

Allow properties of classes and tables to be inferred as write-only.

## Motivation

This RFC is a follow-on to supporting read-only properties.

Read-only properties have many obvious use-cases, but write-only properties
are more technical.

The reason for wanting write-only properties is that it means
that we can infer a most specific type for functions, which we can't do if
we only have read-write and read-only properties.

For example, consider the function
```lua
  function f(t) t.p = Dog.new() end
```

The obvious type for this is
```lua
  f : ({ p: Dog }) -> ()
```

but this is not the most specific type, since read-write properties
are invariant, We could have inferred `f : ({ p: Animal }) -> ()`.
These types are incomparable (neither is a subtype of the other)
and there are uses of `f` that fail to typecheck depending which one choose.

If `f : ({ p: Dog }) -> ()` then
```lua
  local x : { p : Animal } = { p = Cat.new() }
  f(x) -- Fails to typecheck
```

If `f : ({ p: Animal }) -> ()` then
```lua
  local x : { p : Dog } = { p = Dog.new() }
  f(x) -- Fails to typecheck
```

The reason for these failures is that neither of these is the most
specific type. It is one which includes that `t.p` is written to, and
not read from.
```lua
  f : ({ set p: Dog }) -> ()
```

This allows both example uses of `f` to typecheck. To see that it is more specific than `({ p: Animal }) -> ()`:

* `Dog` is a subtype of `Animal`
* so (since write-only properties are contravariant) `{ set p: Dog }` is a supertype of `{ set p: Animal }`
* and (since read-write properties are a subtype of write-only properties)  `{ set p: Animal }` is a supertype of `{ p: Animal }`
* so (by transitivity)  `{ set p: Dog }` is a supertype of `{ set p: Animal }` is a supertype of `{ p: Animal }`
* so (since function arguments are contravariant `({ set p: Dog }) -> ()` is a subtype of `({ p: Animal }) -> ()`

and similarly `({ set p: Dog }) -> ()` is a subtype of `({ p: Dog }) -> ()`.

Local type inference depends on the existence of most specific (and most general) types,
so if we want to use it "off the shelf" we will need write-only properties.

There are also some security reasons why properties should be
write-only. If `t` is a shared table, and any security domain can
write to `t.p`, then it may be possible to use this as a back-channel
if `t.p` is readable. If there is a dynamic check that a property is
write-only then we may wish to present a script analysis error if a
user tries reading it.

## Design

### Properties

Add a modifier to table properties indicating that they are write-only.

This proposal is not about syntax, but it will be useful for examples to have some. Write:

* `set p: T` for a write-only property of type `T`.

For example:
```lua
function f(t)
  t.p = 1 + t.q
end
```
has inferred type:
```
f: (t: { set p: number, get q: number }) -> ()
```
indicating that `p` is used write-only but `q` is used read-only.

### Adding read-only and write-only properties

There are various points where type inference adds properties to types, we now have to consider how to treat each of these.

When reading a property from a free table, we should add a read-only
property if there is no such property already. If there is already a
write-only property, we should make it read-write.

When writing a property to a free table, we should add a write-only
property if there is no such property already. If there is already a
read-only property, we should make it read-write.

When writing a property to an unsealed table, we should add a read-write
property if there is no such property already.

When declaring a method in a table or class, we should add a read-only property for the method.

### Subtyping

Write-only properties are contravariant:

* If `T` is a subtype of `U` then `{ set p: U }` is a subtype of `{ set p: T }`.

Read-write properties are a subtype of write-only properties:

* If `T` is a subtype of `U` then `{ p: U }` is a subtype of `{ set p: T }`.

### Indexers

Indexers can be marked write-only just like properties. In
particular, this means there are write-only arrays `{set T}`, that are
contravariant. These are sometimes useful, for example:

```lua
function move(src, tgt)
  for i,v in ipairs(src) do
    tgt[i] = src[i]
    src[i] = nil
  end
end
```

we can give this function the type
```
  move: <a>({a},{set a}) -> ()
```

and since write-only arrays are contravariant, we can call this with differently-typed
arrays:
```lua
  local dogs : {Dog} = {fido,rover}
  local animals : {Animal} = {tweety,sylvester}
  move (dogs,animals)
```

This program does not type-check with read-write arrays.

### Classes

Classes can also have write-only properties and indexers.

Some Roblox APIs which manipulate callbacks are write-only for security reasons.

### Separate read and write types

Once we have read-only properties and write-only properties, type intersection
gives read-write properties with different types.

```lua
  { get p: T } & { set p : U }
```

If we infer such types, we may wish to present them differently, for
example TypeScript allows both a getter and a setter.

## Drawbacks

This is adding to the complexity budget for users, who will be faced
with inferred set modifiers on many properties.  There is a trade-off
here about how to spend the user's complexity budget: on understanding
inferred types with write-only properties, or debugging false positive
type errors caused by variance issues).

## Alternatives

Just stick with read-only and read-write accesses.
