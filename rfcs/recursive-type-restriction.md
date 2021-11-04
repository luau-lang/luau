# Recursive type restriction

**Status**: Implemented

## Summary

Restrict generic type aliases to only be able to refer to the exact same instantiation of the generic that's being declared.

## Motivation

Luau supports recursive type aliases, but with an important restriction:
users can declare functions of recursive types, such as:
```lua
  type Tree<a> = { data: a, children: {Tree<a>} }
```
but *not* recursive type functions, such as:
```lua
  type Weird<a> = { data: a, children: Weird<{a}> }
```
If types such as `Weird` were allowed, they would have infinite unfoldings for example:
```lua
  Weird<number> = { data: number, children: Weird<{number}> }`
  Weird<{number}> = { data: {number}, children: Weird<{{number}}> }
  Weird<{{number}}> = { data: {{number}}, children: Weird<{{{number}}}> }
  ...
```

Currently Luau has this restriction, but does not enforce it, and instead
produces unexpected types, which can result in free types leaking into
the module exports.

## Design

To enforce the restriction that recursive types aliases produce functions of
recursive types, we require that in any recursive type alias defining `T<gs>`,
in any recursive use of `T<Us>`, we have that `gs` and `Us` are equal.

This allows types such as:
```lua
  type Tree<a> = { data: a, children: {Tree<a>} }
```
but *not*:
```lua
  type Weird<a> = { data: a, children: Weird<{a}> }
```
since in the recursive use `a` is not equal to `{a}`.

This restriction applies to mutually recursive types too.

## Drawbacks

This restriction bans some type declarations which do not produce infinite unfoldings,
such as:
```lua
  type WeirdButFinite<a> = { data: a, children: WeirdButFinite<number> }
```
This restriction is stricter than TypeScript, which allows programs such as:
```typescript
interface Foo<a> { x: Foo<a[]>[]; y: a; }
let x: Foo<number> = { x: [], y: 37 }
```

## Alternatives

We could adopt a solution more like TypeScript's, which is to lazily rather than eagerly instantiate types.
