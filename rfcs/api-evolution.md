# API evolution

## Summary

Add language features to support module API evolution.

## Motivation

API evolution is an important aspect of software. Software is broken
into modules, and those modules can evolve independently. It is
important that developers of reusable modules be aware of when API
changes may break existing uses. This RFC proposes language features
which make it easier to evolve a module's API without breaking
existing uses.  These language features can be supported by tooling
which detect breaking changes.

## Design

### Definition of breaking API change

In order to avoid breaking changes, we need to know what they are!

In this RFC, we follow the [Rust API Evolution](https://rust-lang.github.io/rfcs/1105-api-evolution.html)
guidelines, which build on the [Semantic Versioning](https://semver.org/) specification.
This RFC is just about *API changes* not *behavioral changes* (which are important but out of scope).

From the Rust API evolution guidelines, changes are classified as:

> 1. *Major*: must be incremented for changes that break client code.
> 2. *Minor*: incremented for backwards-compatible feature additions.
> 3. *Patch*: incremented for backwards-compatible bug fixes.

with the requirement that:

> *the same code should be able to run against different minor revisions*. Furthermore, minor changes should require at most a few local annotations to the code you are developing, and in principle no changes to your dependencies.

Adapting this to Luau, minor changes *are* allowed to introduce errors
in type inference, but *are not* allowed to introduce errors in
type-checking code with type annotations.

For example, refining the return type of a function can cause problems with
greedy type inference. If a module changes:

```lua
  function pet() : Animal ... end -- old version
  function pet() : Cat    ... end -- new version
```

then a use relying on type inference may generate errors:

```lua
  local a = pet() -- Inferred as Animal in the old API but Cat in the new API
  if b then
    a = Dog.new() -- Produces a type error in the new API
  end
```

but this does not produce an error when the code is explicitly type annotated:

```lua
  local a: Animal = pet()
  if b then
    a = Dog.new() -- No type error from the new API
  end
```

### Compatibility of exports

For a module's exports, we can make use of Luau's notion of *subtyping*. The basic idea is:

* For a *patch* change, the new type of exports must be *equal* to the old type.
* For a *minor* change, the new type of exports must be *a subtype* of the old type.
* For a *major* change, the new type of exports can be *unrelated* to the old type.

For example, if a module exports:

```lua
  function pet() : Animal -- old version
  function pet() : Cat    -- new version
```

then this is a minor change, since `() -> Cat` is a subtype of `() -> Animal`, but
if it also exports:

```lua
  function adopt(a : Animal) -- old version
  function adopt(c : Cat)    -- new version
```

then this is a major change, since `(Cat) -> ()` is not a subtype of `(Animal) -> ()`.

### Read-only exports

By itself, this is a simple requirement, but is a bit too
restrictive. Firstly, in the absence of [explicit read-only
annotations](property-readonly.md), exported properties are read-write
and so invariant. As a result, the simple definition would never allow
minor changes when a table is exported! For example:

```lua
  local exports = {}
  function exports.pet() : Animal -- old version
  function exports.pet() : Cat    -- new version
  return exports
```

would be considered a breaking change if client assigns to the `pet` property:

```lua
  local A = require(script.parent.A) -- require the above module
  A.pet = function() return Dog.new() end -- type error for the new API
```

For this reason, when checking API compatibility, we consider all properties
of an exported table to be read-only (and hence covariant).

### Instantiate new functions if needed

The second change that is not allowed by the simple definition is
replacing a monomorphic function with a generic one. For example:

```lua
  function addZero(n : number) return 0+n end -- old version
  function addZero(n)          return n   end -- new version
```

This is a simple optimization, and should be allowed, but the new type
is generic `<a>(a) -> a`, but the old type is monomorphic `(number) ->
number`. When the generic function is called, it is *instantiated*,
for example `a` is replaced by `number`, so we should also allow this
when checking API compatibility.

### Exported type aliases

As well as exported values, modules can export type aliases. For example:

```lua
  export type Point = { x: number, y: number }
```

These types are type aliases, and are treated *structurally* not
*nominally*, for example a user can write:

```lua
  local p : Point = { x=5, y=8 }
  local a : number = p.x + p.y
```

and since table types support [width subtyping](sealed-table-subtyping.md), users can add properties:

```lua
  local q : Point = { x=5, y=8, z="hi" }
```

As a result, it is a *breaking change* to change an exported type, for example

```lua
  export type Point = { x: number, y: number            } -- old version
  export type Point = { x: number, y: number, z: number } -- new version
```

would cause type errors in both the initialization of `p` and `q`.
Even adding optional properties is a breaking change, as

```lua
  export type Point = { x: number, y: number             } -- old version
  export type Point = { x: number, y: number, z: number? } -- new version
```

breaks the initialization of `q`.

### Opaque exported types

Any change to a type alias is a breaking change, but this is quite
a strong restriction. Developers may be frustrated that adding properties
to tables requires a major version update.

Languages allow types to evolve by allowing *opaque* types as well as type aliases.
These are types whose definition can be used inside the module, but not externally.

For example, we could allow an opaque type to be declared as
one which may have extra properties, indicated `...`. Within the
module, we know there are no additional properties, but not externally:

```lua
  export type Point = { x: number, y: number, ... }
  function exports.new() : Point return { x=0, y=0 }  end
```

User code cannot construct `Point`s directly, and relies on exported
factory methods. They do have access to the properties though:

```lua
  local p : Point = Point.new()
  p.x = 5; p.y = 7;
  local a : number = p.x + p.y
```

The subtyping rules for an opaque type:

```lua
  export type t<as> = { ps : Ts, ... }
```

are (using `{| ps : Ts |}` for unsealed tables and `{ ps : Ts }` for sealed tables):

* within the defining module, `{| ps : Ts[Us/as] |}` is a subtype of `t<Us>`, and
* anywhere, `t<Us>` is a subtype of `{ ps : Ts[Us/as] }`.

The API evolution rule for an opaque type is width subtyping, for example
it is a minor change to add a property:

```lua
  export type Point = { x: number, y: number,            ... } -- old version 
  export type Point = { x: number, y: number, z: number, ... } -- new version 
  function exports.new() : Point return { x=0, y=0      } end  -- old version
  function exports.new() : Point return { x=0, y=0, z=0 } end  -- new version
```

### Restricting opaque exported types to adding optional properties

Opaque types require all types to have explicit factory methods,
which is restrictive, especially for configuration objects, for example:

```lua
  export type PointConfig = { x: number, y : number }
  export type Point = { x: number, y : number, ... }
  function exports.new(config : PointConfig?) : Point
    local result : Point = { x=0, y=0 }
    if config then
      result.x = config.x
      result.y = config.y
    end
    return result
  else
```

can be used as

```lua
  Point.new({ x=5, y=7 })
```

This is very convenient, but breaks if a new `z` property is added to `Point` and `PointConfig`.
This change is sound, *as long as* the properties added are all optional.

We can support this use case by allowing opaque types where we require that any new properties are optional,
written `...?`. For example:

```lua
  export type PointConfig = { x: number, y : number,             ...? } -- old
  export type PointConfig = { x: number, y : number, z: number?, ...? } -- new
```

with matching changes to the rest of the API. Existing callers of the old API still work,
but users can also rely on the new API, for example:

```lua
  Point.new({ x=5, y=7, z=9 })
```

The subtyping rules for an opaque type:

```lua
  export type t<as> = { ps : Ts, ...? }
```

are:

* anywhere, `{| ps : Ts[Us/as] |}` is a subtype of `t<Us>`, and
* anywhere, `t<Us>` is a subtype of `{ ps : Ts[Us/as] }`.

The API evolution rule is width subtyping, with the restriction that all new properties
have an optional type.

This is sound because unsealed tables can have new optional properties added to them, for example
`{| p: T |}` is a subtype of `{| p: T, q: U? |}`.

### Bounded existential types

The theory behind modules with opaque types is bounded existential
types, for example the type of the module:

```lua
  local exports = {}
  export type Point = { x : number, y : number, ... }
  function exports.new() : Point return { x=0, y=0 } end
```

is:

```
  ∃ (Point ≤ { x : number, y : number}) .
    { new : () → Point }
```

and the type of the module:

```lua
  local exports = {}
  export type PointConfig = { x : number, y : number, ...? }
  export type Point = { x : number, y : number, ... }
  function exports.new(config : PointConfig?) : Point ... end
```

is:

```
  ∃ ({| x : number, y : number |} ≤ PointConfig ≤ { x : number, y : number}) .
  ∃ (Point ≤ { x : number, y : number}) .
    { new : () → Point }
```

For details about existential types, see (*Types And Programming
Languages*)[https://www.cis.upenn.edu/~bcpierce/tapl/].

The particular variant of existential types being proposed here is as
in (*An Existential Crisis
Resolved*)[https://dl.acm.org/doi/10.1145/3473569], in particular the
treatment of `require` as as in `open`, not `unpack`.  This is not
sound in general for Luau, since Luau is nondeterministic, but since
`require` expressions use module *paths* (which are deterministic)
rather than *expressions* (which are not) the system is sound (see the
discussion of module systems in §10 of that paper for more details).

## Drawbacks

There is the usual tradeoff of complexity versus flexibility here.
In particular, `...` and `...?` are adding two extra notions,
but they both seem useful.

## Alternatives

There are the usual bike-shedding options for syntax, in particular
we could use attributes rather than new syntax for `...` and `...?`.

We could allow other privacy modifiers, for example declaring some
properties private, or allowing package-level privacy scope as well
as module-level.

We could add extra subtyping rules for opaque types, for example
allowing library authors to explicitly declare subtyping between
opaque types, or variance annotations on type parameters.