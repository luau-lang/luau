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
These examples come up naturally in OO code bases with generic types, for example
```lua
--!strict
export type PromiseFor<T> = {
  andThen: <U>(
    self: PromiseFor<T>,
    onFulfilled: ((result: T) -> U)?,
    onRejected: ((err: string) -> U)?
  ) -> PromiseFor<U>,
  catch: <U>(
    self: PromiseFor<T>,
    onRejected: ((err: string) -> U)?
  ) -> PromiseFor<U>,
  finally: <U>(
    self: PromiseFor<T>,
    onResolvedOrRejected: ((wasFulfilled: boolean, resultOrErr: T | string) -> U)
  ) -> PromiseFor<U>,
}
```
as discussed at the [Roblox DevForum](https://devforum.roblox.com/t/regression-with-genericrecursively-defined-types/1616647).

Examples like this are quite common in TypeScript code, for example in [ReduxJS](https://github.com/reduxjs/redux-thunk/blob/master/src/types.ts).

## Design

The design is to continue to use strict type aliases, but to use a
cache during type alias definition. Type aliases are handled as they
currently are, except for recursive cases.

When defining a type alias `T<a1,...,aN>`, if we encounter a recursive use
`T<U1,...,UN>` we proceed as follows:

* If every `UI` is `aI`, or if every `UI` does not contain any of the `aJ`s:
    * look `<U1,...,UN>` up in the cache,
    * if the cache lookup succeeds, return the cached result,
    * otherwise create a fresh type variable, add it to the cache, and return it.

* Otherwise, produce a type error and return an error type.

Once we are finished defining the type alias, iterate through the cache

* for each entry with key `<U1,...,UN>` and value `X`, unify `X` with
  the result of expanding `T<U1,...,UN>`.

* expanding `T<U1,...,UN>` may encounter a generic function,
  for example `<b>(S) -> R`, which needs a bit of care, since `a` may occur free in
  some of the `Ui`. We need to rename `b` to `c`, but this renaming may also
  introduce new cache entries, since any cache entry for `U` containing `c`
  needs a new cache entry for `U[c/b]`.

For example, with type

```
  type TreeWithMap<a> = { data: a, children: {Tree<a>}, map: <b>(a -> b) -> Tree<b> }
```

The type alias is `Z` where

```
  Z = { data: a, children: {Z}, map: <b>(a -> b) -> X }
```

the cache contains

```
  <b> |-> X
```

and after unification

```
  X = { data: b, children: {X}, map: <c>(b -> c) -> Y }
```

which introduces a new cache entry

```
  <c> |-> Y
```

and after unification

```
  Y = { data: b, children: {Y}, map: <b>(c -> b) -> X }
```

This algorithm can be generalized to handle mutual recursion, by using a cache for each mutually recursive type.

This design is the strictest one we came up with that copes with the examples we're interested in, in particular the `PromiseFor` example.

## Drawbacks

Renaming can double the size of the type graph, with the result that nested type aliases can result in exponential blowup.

This algorithm doesn't cope with examples which mix concrete and generic types
```
  type T<a, b> = { this: a, that; b, children : {T<number, b>} }
```

This algorithm does not support type graphs with infinite expansions.

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

We can use occurrence checks to ensure there's less blowup. We can restrict
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

This still has exponential blowup, for example
```lua
type T<a1,a2,a3,...,aN> = {
  p1: T<number, a2, a3, ..., aN>,
  p2: T<a1, number, a3, ..., aN>,
  ...
  pN: T<a1, a2, a3, ..., number>,
}
```

*Advantages*: types are computed strictly, and may produce better error messages if the occurs check fails.
