# Shared self types

## Summary

This RFC proposes sharing `self` types between method definitions which share a metatable.

## Motivation

Currently, metamethods are type-inferred independently, and so give
completely separate types to `self`. This has poor ergonomics, as type
errors for inconsistent methods are produced on method calls rather
than method definitions. It also has poor performance, as the independent
self types have separate memory footprints, and there is idiomatic code with
exponential blowup in the size of the type graph. 

For example, `Point` class can be simulated using metatables:

```lua
  --!strict

  local Point = {}
  Point.__index = Point

  function Point.new()
    local result = {}
    setmetatable(Point, result)
    result.x = 0
    result.y = 0
    return result
  end

  function Point:getX()
    return self.x
  end

  function Point:getY()
    return self.Y
  end

  function Point:abs()
    return math.sqrt(self:getX() * self:getX() + self:getY() * self:getY())
  end
```

Currently, this code is problematic, since there is no connection between the types
of the `Point` metamethods. For example, the inferred type for `Point.getX`
is `<a>({ x : a }) -> a`, rather than the expected `(Point) -> number`.

Even worse, the method `Point.abs` does not type-check, since the type
of `self.x * self.x` is unknown. If Luau had subtyping constraints and type families
for overloaded operators, the inferred type would be something like:

```lua
  type PointMT = {
    new : () -> Point,
    getX : <a>({ x : a }) -> a,
    getY : <a>({ y : a }) -> a,
    abs : <a, b, c>(a) -> number where
      a <: { getX : (a) -> b, getY : (a) -> c }
      Add<Mul<b, b>, Mul<c, c>> <: number,
  }
  type Point = {
    x : number,
    y : number,
    @metatable PointMT
  }
```

but this type is not great ergonomically, since this type may be presented to
users in type hover or type error messages, and will surprise users
expecting a simpler type such as:

```lua
  type PointMT = {
    new : () -> Point
    getX : (Point) -> number,
    getY : (Point) -> number,
    abs : (Point) -> number
  }
  type Point = {
    x : number,
    y : number,
    @metatable PointMT
  }
```

This is the type inferred by *shared self types*. Rather than
inferring the `self` type separately for each metamethod declared on a
table, and for each use of `setmetatable` the same type is used.

Unfortunately, while this change is fairly straightforward for
monomorphic types like `Point`, it is problematic for generic classes
such as containers. For example:

```lua
local Set = {}
Set.__index = Set

function Set.new()
    return setmetatable({
        elements={}
    }, Set)
end

function Set:add(el)
    self.elements[el] = true
end

function Set:contains(el)
    return self.elements[el] != nil
end
```

In this case, the expected type would be something like:


```lua
  type SetMT = {
    new : <E>() -> Set<E>,
    add : <E>(Set<E>, E) -> (),
    contains : <E>(Set<E>, E) -> boolean
  }
  type Set<E> = {
    elements : { [E] : boolean },
    @metatable SetMT
 }
```

Inferring this type is beyond the scope of this RFC, though. Initially, we propose only inferring `self` monotypes,
in this case:


```lua
  type SetMT = {
    new : () -> Set,
    add : (Set, unknown) -> (),
    contains : (Set, unknown) -> boolean
  }
  type Set = {
    elements : { [unknown] : boolean },
    @metatable SetMT
  }
```

and propose allowing explicit declaration of the shared self type,
following the common practice of naming the self type after the metatable:

```lua
  type Set<E> = { elements : { [E] : boolean } }
```

This type (and its generic type parameters) are used to derive the
type of `self` in methods declared using `function Set:m()`
declarations:

```lua
  type SetSelf<E> = {
    elements : { [E] : boolean },
    @metatable SetMT
  }
```

In cases where shared self types are just getting in the way, there
are two work-arounds. Firstly, the shared self type can be declared to
be `any`, which will silence type errors:

```lua
  type Foo = any
```

Secondly, the self type can be declared explicitly:

```lua
  function Foo.m(self : Bar) ... end
```

## Design

### Self types

For each table `t`, introduce:

* the self type parameters of `t`, a sequence of generic type and typepack variables,
* the self type definition of `t`, a type which can use the type parameters of `t`, and
* the self type of `t`, a type which can use the type parameters of `t`.

These can be declared explicitly:

```lua
  type t<As> = U
```

which defines, when `t` has type `T`:

* the self type parameters of `t` to be `As`,
* the self type definition of `t` to be `U`, and
* the self type of `t` to be `U` extended with `@metatable T`.

For example,

```lua
  type Set<E> = { [E] : boolean }
```

declares, when `Set` has type `SetMT`:

* the self type parameters of `Set` to be `E`,
* the self type definition of `Set` to be `{ [E] : boolean }`, and
* the self type of `Set` to be `{ [E] : boolean, @metatable SetMT }`.

If there is no explicit declaration of the self type of `t`, then, when `t` has type `T`:

* the self type parameters of `t` are empty,
* the self type definition of `t` is a free table type `U`,
* if `t` has an `__index` property of type `MT`, then the self type of `t` is
  a metatable type, whose metatable is `MT`, and whose table is `U`, and
* if `t` does not have an `__index` property, then the self type of `t` is `U`.

The free table type is unified in the usual fashion.

Self types are used in two ways: in calls to `setmetatable`, and in metamethod declarations.

### `setmetatable`

In calls to `setmetatable(t, mt)`:

* if `mt` has self type parameters `As`, self type definition `T` and self type `S`,
* and `T` has (final) type `T [ Ts/As ]`
* then `setmetatable(t, mt)` has type `S [ Ts/As ]`.

As currently, this has a side-effect of updating the type state of `t` from
`T [ Ts/As ]` to `S [ Ts/As ]`.

For example, in `setmetatable({ elements = {} }, Set)`, we have:

* `Set` has type `SetMT`, self type parameter `E`, self type definition` { elements : { [E] : boolean } }`, and self type `{ elements : { [E] : boolean }, @metatable SetMT }`,
* and `{ elements = {} }` has type `{ elements : { [X] : boolean } }` for a free `X`,
* so `setmetatable({ elements = {} }, Set)` has type `{ elements : { [X] : boolean }, @metatable SetMT }`.

as a result `Set.new` has type `<a>() -> { elements : { [a] : boolean }, @metatable SetMT }`.

As another example, in `setmetatable(Point, result)`:

* `Point` has type `PointMT`, no self type parameters, a free self type definition (call it `T`), and a self type whose table type is `T` and whose metatable is `PointMT`,
* and `result` has final type `{ x : number, y : number }`,
* so (by unifying `T` with `{ x : number, y : number }`) `setmetatable(Point, result)` has type `{ x : number, y : number, @metatable PointMT }`.

As a side-effect, the type state of `result` is updated to be `{ x : number, y : number, @metatable PointMT }`,
and unification causes the self type of `Point` to be `{ x : number, y : number, @metatable PointMT }`.

Note that this relies on the type of `result` being `{ x : number, y : number }`, which is why we use the final
long-lived type of `t` rather than its current type state.

### Method declarations

In method declarations `function mt:m`:

* if `mt` has self type parameters `As` and self type `S`,
* then give the `self` parameter to `MT.m` the type `S [ Xs/As ]` for fresh free Xs (these types are quantified as all other free types are).

For example, in the method `Set:add(el)`:

* `Set` has self type parameter `E`, and self type `{ elements : { [E] : boolean }, @metatable SetMT }`,
* so `self` has type `{ elements : { [X] : boolean }, @metatable SetMT }` when type checking the body of `Set:add(el)`.

At this point, type inference proceeds as usual:

* `el` is given fresh free type `Y`,
* the statement `self.elements[el] = true` will unifies `X` and `Y`, and
* quantifying results in type `<a>({ elements : { [a] : boolean }, @metatable SetMT }) -> ()`.

## Drawbacks

### Partially-constructed objects

Shared self types capture an idiom where tables with metatables have
all of their fields initialized *before* any methods are called. In
cases where methods are called before all the fields are initialized,
this will result in optional types being inferred. For example:

```lua
  function Point.new()
    local result = setmetatable(Point, {})
    result.x = 0
    print(result:getX())
    result.y = 0
    print(result:getY())
    return result
  end
```

the call to `result:getY()` uses the *current* type state of `result`, which is `{ x : number, @metatable PointMT }`.
Unification will then cause `Point` to consider `y` to be optional:

```lua
  type PointMT = {
    new : () -> Point
    getX : (Point) -> number,
    getY : (Point) -> number?,
    abs : (Point) -> number -- with a type error!
  }
  type Point = {
    x : number,
    y : number?,
    @metatable PointMT
  }
```

Since `y` has type `number?` rather than `number`, the `abs` method will fail to type check.

As a workaround, developers can declare different self types for different methods:

```lua
  function Point.getX(self : { x : number }) : number
    return self.x
  end
  function Point.getY(self : { y : number }) : number
    return self.y
  end
```

resulting in:

```lua
  type PointMT = {
    new : () -> Point
    getX : ({ x : number }) -> number,
    getY : ({ y : number }) -> number,
    abs : (Point) -> number -- without a type error!
  }
  type Point = {
    x : number,
    y : number,
    @metatable PointMT
  }
```

or can switch off type checking `self` by declaring `type Point = any`.

### Methods called on both tables and metatables.

This is a similar problem, caused by calling methods directly on
metatables as well as tables. For example calling `Point:abs()` will
result in inferring that both `x` and `y` are optional,

## Alternatives

We could use new syntax for declaring self types, rather than using
the convention that they have the same name as the metatable.

We could do nothing, but at a performance and ergonomics cost.

We could introduce special syntax for classes or records, though this
doesn't address type checking current code.
