# Property getter and setter types

## Summary

Allow properties of classes and tables to have separate types for
read access and write access.

## Motivation

Currently, Roblox APIs have read-only properties of classes, but our
type system does not track this. As a result, users can write (and
indeed due to autocomplete, an encouraged to write) programs with
run-time errors.

In addition, user code may have properties (such as module exports)
that are expected to be used without modification. Currently there is
no way for user code to indicate this, even if it has explicit type
annotations.

It is very common for functions to only require read access to a parameter,
and this can be inferred during type inference.

There are also technical benefits to separating read and write access:
read access is covariant, but read-write access
is invariant. Since Luau type inference
depends on subtyping, this can simplify type-checking considerably.

## Design

### Properties

In this proposal, we separate the *getter type* of a property from its
*setter type*. Properties may have both a getter and a setter type, and both are optional.

This proposal is not about syntax, but it will be useful for examples to have some. Write:

* `get p: T` for a property with getter type `T`, and
* `set p: T` for a property with setter type `T`.

A property can have both a getter and a setter type. The common case is
read-write access at the same type:

* `p: T`is the same as `get p: T, set p: T`.

For example:
```lua
function f(t)
  t.r = math.sqrt(t.p)
  t.p = t.p + t.q
end
```
has inferred type:
```
f: (t: { p: number, get q: number, set r: number }) -> ()
```
indicating that `p` is used read-write, `q` is used read-only, and `r` is used write-only.

### Indexers

Indexers can be marked `get` or `set` just like properties. In
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
function t.f() ... end
function t:m() ... end
```
should introduce a getter, but not a setter. If developers want a mutable function,
they can use the anonymous function version
```lua
t.f = function() ... end
```

Methods in classes should be read-only by default.

*This is a possibly breaking change.*

### Classes

Getter and setter types apply to classes as well as tables.

Many of the Roblox APIs an be marked as having getters but not
setters, which will improve accuracy of type checking for Roblox APIs.

### Metatables

Metatables should be read-only by default.

*This is a possibly breaking change.*

### Require

A `require`d module's properties should be read-only by default.

*This is a possibly breaking change.*

### Separate getter and setter for Instance.Parent

If we had separate getters and setters for Instance.Parent, we could
allow setting with any `Instance`, but getting at the type given by
the initial DM. This is unsound, but would support reparenting, and
any solution for reparenting is either going to require whole-program
analysis or have unsoundness.

### Why separate the getter from the setter?

Separate getters and setters were introduced to TypeScript in response
to use-cases of APIs which performed data conversion using getter and
setter methods (the equivalent of Lua `__index` and `__newindex`
metamethods).

As well as those use-cases, separating read and write access,
considerably simplifies variance, which is an important part of type inference:

* Read-only properties are *covariant*: if `T` is a subtype of `U`
  then `{ get p: T }` is a subtype of `{ get p: U }.
* Write-only properties are *contravariant*: if `T` is a supertype of `U`
  then `{ set p: T }` is a subtype of `{ set p: U }.

This allows type inference to provide a principal type to tables.
For example in:

```lua
local x = { p: nil }
function g(d : Dog?) x.p = d end
function h(): Animal? return x.p end
```

it is not obvious what type to infer for `x`, should it be `{ p: Dog? }` or `{ p: Animal? }`?
With either of those types, the following code does not typecheck:

```lua
local t1: { p: Animal? } = { p: nil }
local t2: { p: Dog? } = { p: nil }
x = t1 -- Does not typecheck if x: { p: Dog? }
x = t2 -- Does not typecheck if x: { p: Amimal? }
```

If there are separate getter and setter types, there is a most general type, which is
`{ get p: Animal?, set p: Dog? }`, which allows this program to typecheck.

With separate getter and setter types, every position in a type is either
covariant or contravariant, and *there are no more uses of invariance*.
This simplifies the implementation (all our special-casing for
invariance is no longer needed) and makes possible techniques such as local
type inference (which relies on variance to infer a principal type).

### Related work

This proposal is based on
[TypeScript 4.3](https://devblogs.microsoft.com/typescript/announcing-typescript-4-3/#separate-write-types).
getters and setters.

## Drawbacks

This is adding to the complexity budget for users,
who will be faced with get/set modifiers on many properties.

## Alternatives

Rather than making read-write access the default, we could make read-only the
default and add a new modifier for read-write. This is not backwards compatible.

Rather than separate getter and setter types, we could have one type
with a read/write annotation. This would re-introduce invariance, and
would not allow the example program to typecheck.

There is a trade-off here, between reporting a type error in cases
like this, or inferring a type with separate getters and setters. Both
of these introduce complexity:

* If we report a type error, the user has to work out why this is an
  error, and how to fix it. In order to understand why it is an error,
  the user needs to understand subtyping and the variance rules for
  properties.
  
* If we infer separate getters and setters, the user will be faced
  with types with separate getters and setters in error messages and
  type hover.

This is a tricky trade-off, as we are deciding for the user how to
spend their complexity budget.

