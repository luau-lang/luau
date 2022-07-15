# Disallow `name T` and `name(T)` in future syntactic extensions for type annotations

## Summary

We propose to disallow the syntax ``<name> `('`` as well as `<name> <type>` in future syntax extensions for type annotations to ensure that all existing programs continue to parse correctly. This still keeps the door open for future syntax extensions of different forms such as ``<name> `<' <type> `>'``.

## Motivation

Lua and by extension Luau's syntax is very free form, which means that when the parser finishes parsing a node, it doesn't try to look for a semi-colon or any termination token e.g. a `{` to start a block, or `;` to end a statement, or a newline, etc. It just immediately invokes the next parser to figure out how to parse the next node based on the remainder's starting token.

That feature is sometimes quite troublesome when we want to add new syntax.

We have had cases where we talked about using syntax like `setmetatable(T, MT)` and `keyof T`. They all look innocent, but when you look beyond that, and try to apply it onto Luau's grammar, things break down fast.

### `F(T)`?

An example that _will_ cause a change in semantics:

```
local t: F
(u):m()
```

where today, `local t: F` is one statement, and `(u):m()` is another. If we had the syntax for `F(T)` here, it becomes invalid input because it gets parsed as

```
local t: F(u)
:m()
```

This is important because of the `setmetatable(T, MT)` case:

```
type Foo = setmetatable({ x: number }, { ... })
```

For `setmetatable`, the parser isn't sure whether `{}` is actually a type or an expression, because _today_ `setmetatable` is parsed as a type reference, and `({}, {})` is the remainder that we'll attempt to parse as a statement. This means `{ x: number }` is invalid table _literal_. Recovery by backtracking is technically possible here, but this means performance loss on invalid input + may introduce false positives wrt how things are parsed. We'd much rather take a very strict stance about how things get parsed.

### `F T`?

An example that _will_ cause a change in semantics:

```
local function f(t): F T
    (t or u):m()
end
```

where today, the return type annotation `F T` is simply parsed as just `F`, followed by a ambiguous parse error from the statement `T(t or u)` because its `(` is on the next line. If at some point in the future we were to allow `T` followed by `(` on the next line, then there's yet another semantic change. `F T` could be parsed as a type annotation and the first statement is `(t or u):m()` instead of `F` followed by `T(t or u):m()`.

For `keyof`, here's a practical example of the above issue:

```
type Vec2 = {x: number, y: number}

local function f(t, u): keyof Vec2
    (t or u):m()
end
```

There's three possible outcomes:

1. Return type of `f` is `keyof`, statement throws a parse error because `(` is on the next line after `Vec2`,
2. Return type of `f` is `keyof Vec2` and next statement is `(t or u):m()`, or
3. Return type of `f` is `keyof` and next statement is `Vec2(t or u):m()` (if we allow `(` on the next line to be part of previous line).

### But what about `typeof(...)`?

This syntax is grandfathered in because the parser supported `typeof(...)` before we stabilized our syntax, and especially before type annotations were released to the public, so we didn't need to worry about compatibility here. We are very glad that we used parentheses in this case, because it's natural for expressions to belong within parentheses `()`, and types to belong within angles `<>`.

## Alternatives

Only allow these syntax when used inside parentheses e.g. `(F T)` or `(F(T))`. This makes it inconsistent with the existing `typeof(...)` type annotation, and changing that over is also breaking change.

Support backtracking in the parser, so if `: MyType(t or u):m()` is invalid syntax, revert and parse `MyType` as a type, and `(t or u):m()` as an expression statement. Even so, this option is terrible for:
  1. parsing performance (backtracking means losing progress on invalid input),
  2. user experience (why was this annotation parsed as `X(...)` instead of `X` followed by a statement `(...)`),
  3. has false positives (`foo(bar)(baz)` may be parsed as `foo(bar)` as the type annotation and `(baz)` is the remainder to parse)

## Drawbacks

To be able to expose some kind of type-level operations using `F<T>` syntax, means one of the following must be chosen:
  1. introduce the concept of "magic type functions" into type inference, or
  2. introduce them into the prelude as literally `export type F<T> = intrinsic`
