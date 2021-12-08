# Feature name

## Summary

We need syntax to match the semantics of read-only and writed-only modifiers for table properties and indexers.

## Motivation

See the semantic RFCs for motivation.

## Design

TBD

## Drawbacks

TBD

## Alternatives

The design space for syntax includes:

* Names, symbols or attributes?
* Modifier position?
* How to parse or serialize properties with differing read- and write-types?

### Names, symbols or attributes?

We could use names for modifiers, such as

* `get` and `set`
* `const` and `mut`
* `read` and `write`
* `readonly` and `writeonly`

One issue is that these are all valid identifiers, so if we want
backward compatbility, they cannot be made keywords. This presents
issues with code that uses the chosen names as type or property names,
for instance:

```lua
  type set = { [any] : bool }
  type ugh = { get set : set }
```

We could use symbols, for example

* `+` and `-`
* (Are there other obvious pairs of symbols?)

We could use attributes, for example

* `@get` and `@set`
* ... as per "names for modifiers", only prefixed by `@` ...

These both have the advantage of being unambiguous and easier to
parse. Symbols are terser, whch is both good and bad.

### Modifier position?

For attributes, the position is given by the syntax of attributes, for example:

```lua
  type Vector2 = { @get x: number, @get y : Number }
```

For the other proposals, there are four possibilities, depending on whether
the modifier is west-coast or east-coast, and whether it modifies the propertry name or the type:

```lua
  type Vector2 = { get x : number, get y : number }
  type Vector2 = { x get : number, y get : number }
  type Vector2 = { x : get number, y : get number }
  type Vector2 = { x : number get, y : number get }
```

The east-coast options are not easy-to-reade with names, but are
easier with symbols, especially since `T?` is already postfix, for
example

```lua
  type Foo = { p: number?+ }
```

### How to parse or serialize properties with differing read- and write-types?

One corner case is that type inference may deduce different read- and
write-types, which need to be presented to the user. For example the
read-type of `x` is `Animal` but its write-type is `Dog` in the principal type of:

```lua
   function f(x)
     let a: Animal = x.pet
     x.pet = Dog.new()
     return a
   end
```

If we are adding the modifier to the property name, we can repeat the name, for example

```lua
  x : { get pet : Animal, set pet : Dog }
```

If we are adding the modifier to the property type, we can give both types, for example:
```lua
  x : { pet : get Animal + set Dog }
```

This syntax plays well with symbols for modifiers, for example
```lua
  x : { pet : +Animal -Dog }
```
