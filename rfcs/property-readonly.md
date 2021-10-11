# Read-only properties

## Summary

Allow properties of classes and tables to be inferred as read-only.

## Motivation

Currently, Roblox APIs have read-only properties of classes, but our
type system does not track this. As a result, users can write (and
indeed due to autocomplete, an encouraged to write) programs with
run-time errors.

In addition, user code may have properties (such as methods)
that are expected to be used without modification. Currently there is
no way for user code to indicate this, even if it has explicit type
annotations.

It is very common for functions to only require read access to a parameter,
and this can be inferred during type inference.

## Design

### Properties

Add a modifier to table properties indicating that they are read-only.

This proposal is not about syntax, but it will be useful for examples to have some. Write:

* `get p: T` for a read-only property of type `T`.

For example:
```lua
function f(t)
  t.p = 1 + t.p + t.q
end
```
has inferred type:
```
f: (t: { p: number, get q: number }) -> ()
```
indicating that `p` is used read-write but `q` is used read-only.

### Subtyping

Read-only properties are covariant:

* If `T` is a subtype of `U` then `{ get p: T }` is a subtype of `{ get p: U }`.

Read-write properties are a subtype of read-only properties:

* If `T` is a subtype of `U` then `{ p: T }` is a subtype of `{ get p: U }`.

### Indexers

Indexers can be marked read-only just like properties. In
particular, this means there are read-only arrays `{get T}`, that are
covariant, so we have a solution to the "covariant array problem":

```lua
local dogs: {Dog}
function f(a: {get Animal}) ... end
f(dogs)
```

It is sound to allow this program, since `f` only needs read access to
the array, and `{Dog}` is a subtype of `{get Dog}`, which is a subtype
of `{get Animal}`.  This would not be sound if `f` had write access,
for example `function f(a: {Animal}) a[1] = Cat.new() end`.

### Functions

Functions are not normally mutated after they are initialized, so
```lua
local t = {}
function t.f() ... end
function t:m() ... end
```

should have type
```
t : {
  get f : () -> (),
  get m : (self) -> ()
}
```

If developers want a mutable function,
they can use the anonymous function version
```lua
t.g = function() ... end
```

For example, if we define:
```lua
  type RWFactory<A> = { build : () -> A }
```

then we do *not* have that `RWFactory<Dog>` is a subtype of `RWFactory<Animal>` 
since the build method is read-write, so users can update it:
```lua
  local mkdog : RWFactory<Dog> = { build = Dog.new }
  local mkanimal : RWFactory<Animal> = mkdog -- Does not typecheck 
  mkanimal.build = Cat.new -- Assigning to methods is OK for RWFactory
  local fido : Dog = mkdog.build() -- Oh dear, fido is a Cat at runtime
```

but if we define:
```lua
  type ROFactory<A> = { get build : () -> A }
```

then we do have that `ROFactory<Dog>` is a subtype of `ROFactory<Animal>` 
since the build method is read-write, so users can update it:
```lua
  local mkdog : ROFactory<Dog> = { build = Dog.new }
  local mkanimal : ROFactory<Animal> = mkdog -- Typechecks now!
  mkanimal.build = Cat.new -- Fails to typecheck, since build is read-only
```

Since most idiomatic Lua does not update methods after they are
initialized, it seems sensible for the default access for methods should
be read-only.

*This is a possibly breaking change.*

### Classes

Classes can also have read-only properties and accessors.

Methods in classes should be read-only by default.

Many of the Roblox APIs an be marked as having getters but not
setters, which will improve accuracy of type checking for Roblox APIs.

## Drawbacks

This is adding to the complexity budget for users,
who will be faced with inferred get modifiers on many properties.

## Alternatives

Rather than making read-write access the default, we could make read-only the
default and add a new modifier for read-write. This is not backwards compatible.

We could continue with read-write access to methods,
which means no breaking changes, but means that users may be faced with type
errors such as "`Factory<Dog>` is not a subtype of `Factory<Animal>`".
