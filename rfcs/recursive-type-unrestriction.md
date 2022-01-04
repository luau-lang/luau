# Loosening the recursive type restriction

## Summary

Luau supports recursive type aliases, but with an important
restriction: users can declare functions of recursive types, but *not*
recursive type functions. This has problems with sophisticated uses of
types, for example ones which mix recursive types and nested generics.
This RFC proposes loosening this restriction.

## Motivation

Luau supports recursive type aliases, but with an important restriction:
users can declare functions of recursive types, such as:
```lua
  type Tree<a> = { data: a, children: {Tree<a>} }
```
but *not* recursive type functions, such as:
```lua
  type TreeWithMap<a> = { ..., map: <b>(a -> b) -> Tree<b> }

```
These examples come up naturally in OO code bases with generic types.

## Design

*This section to be filled in once we decide which alternative to use*

## Drawbacks

*This section to be filled in once we decide which alternative to use*

## Alternatives

### Lazy recursive type instantiations

The most permissive change would be to make recursive type
instantiation lazy rather than strict. In this approach `T<U>` would
not be instantiated immediately, but only when the body is needed. In
particular, during unification we can unify `T<U>` with `T<V>` by
first trying to unify `U` and `V`, and only if that fails try to unify
the instantiations.

*Advantages*: this allows recursive types with infinite expansions like:
```lua
  type Foo<T> = { ..., promises: {Foo<Promise<T>>} }
```

### Lazy recursive type instantiations with a cache

As above, but keep a cache for each type function.

*Advantages*: reduces the size of the type graph.

### Strict recursive type instantiations with a cache

Rather than lazily instantiating type functions when they are used, we
could carry on instantiating them when they are defined, and use a
cache to reuse them. In particular, the cache would be populated when the
recursive types are defined, and used when types are used recursively.

For example:
```
type T<a,b> = { foo: T<b,number>? } 
```
would result in cache entries:
```
T<a,b> = { foo: T<b,number>? }
T<b,number> = { foo: T<number,number>? }
T<number,number> = { foo: T<number,number>? }
```
This can result in exponential blowup, for example:
```
type T<a,b> = { foo: T<b,number>?, bar: T<b,string>? } 
```
would result in cache entries:
```
T<a,b> = { foo: T<b,number>?, bar: T<b,string>? }
T<b,number> = { foo: T<number,number>?, bar: T<string,number>? }
T<b,string> = { foo: T<string,number>?, bar: T<string,string>? }
T<number,number> = { foo: T<number,number>?, bar: T<number,string>? }
T<number,string> = { foo: T<string,number>?, bar: T<string,string>? }
T<string,number> = { foo: T<number,number>?, bar: T<number,string>? }
T<string,string> = { foo: T<string,number>?, bar: T<string,string>? }
```
Applying this to a type function with N type variables results in more than 2^N
types. Because of blowup, we would need a bound on cache size.

This can also result in the cache being exhausted, for example:
```
type T<a> = { foo: T<Promise<a>>? } 
```
results in an infinite type graph with cache:
```
T<a> = { foo: T<Promise<a>>? } 
T<Promise<a>> = { foo: T<Promise<Promise<a>>>? } 
T<Promise<Promise<a>>> = { foo: T<Promise<Promise<Promise<a>>>>? }
...
```

*Advantages*: types are computed strictly, so we don't have to worry about lazy types
producing unbounded type graphs during unification.

### Strict recursive type instantiations with a cache and an occurrence check

We can use occurrence checks to ensure there's no blowup. We can restrict
a recursive use `T<U1,...,UN>` in the definition of `T<a1...aN>` so that either `UI` is `aI`
or contains none of `a1...aN`. For example this bans
```
type T<a> = { foo: T<Promise<a>>? } 
```
since `Promise<a>` is not `a` but contains `a`, and bans
```
type T<a,b> = { foo: T<b,number>? } 
```
since `a` is not `b`, but allows:
```
type T<a,b> = { foo: T<a,number>? } 
```

*Advantages*: types are computed strictly, and may produce better error messages if the occurs check fails.
