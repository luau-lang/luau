# Reserve ``<name> `('`` syntax in type annotations

## Summary

We propose to reserve the syntax ``<name> `('`` in type annotations, regardless of where they show up, e.g. a type or a type pack.

## Motivation

Lua and by extension Luau's syntax is very free form, which means that when the parser finishes parsing a node, it doesn't try to look for a semi-colon or any termination token e.g. a `{` to start a block, or `;` to end a statement, or a newline, etc. It just immediately invokes the next parser to figure out how to parse the next node based on the remainder's starting token.

That feature is sometimes quite troublesome when we want to add new syntax.

We have cases where we want to add a new contextual keyword before type annotations, e.g. `setmetatable(...` and `keyof T`. By itself, some of these looks innocent, but type annotations support parentheses, so `keyof (({ x: number } & { x: string | number }) | { y: string })` may be parsed incorrectly.

Note: this RFC does not propose any new contextual keywords.

## Drawbacks

This is a breaking change syntactically, e.g. there may already be code that looks like

```
local function f(t, u): MyType
  (t or u):m()
end
```

If `MyType` ever becomes part of the grammar, oops, the return type of `f` is now parsed like `MyType(t` and becomes invalid syntax when it encounters `or`.

To workaround this, the correct syntax is

```
local function f(t, u): (MyType)
  (t or u):m()
end
```

## Alternatives

Only allow these syntax when used inside parentheses e.g. `(name(...))`, but this makes it inconsistent with the existing `typeof(...)` type annotation, and changing that over is also breaking change. It's pretty clear that anything inbetween not doing this RFC or doing some backward-compatible option adds warts to the grammar.

Support backtracking in the parser, so if `: MyType(t or u):m()` is invalid syntax, revert and parse `MyType` as a type, and `(t or u):m()` as an expression statement. Even so, this option is terrible for:
  1. parsing performance (backtracking means losing progress on invalid input),
  2. user experience (why was this annotation parsed as `X(...)` instead of `X` followed by a statement `(...)`),
  3. has false positives (`foo(bar)(baz)` may be parsed as `foo(bar)` as the type annotation and `(baz)` is the remainder to parse)
