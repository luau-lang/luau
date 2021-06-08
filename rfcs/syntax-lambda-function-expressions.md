# Lambda function expressions 

## Summary

Introduce a way to define anonymous functions using a more terse syntax.

## Motivation

Current function syntax takes up a lot of space when used for short anonymous that are passed in function calls.

By introducing a shorter syntax, we make code with heavy use of anonymous functions easier to write and arguably, easier to read as well:
```lua
local a = sort(c, function(a, b) return a.x > b.x end)
local b = filter(map(c, function(a) return a * 2 end), function(a) return a > 0 end)
```

Proposed:
```lua
local a = sort(c, (a, b) -> a.x > b.x)
local b = filter(map(c, a -> a * 2), a -> a > 0)
```
Type annotations are supported are also supported, but with improvements to bi-directional type inference, typechecker should be able to deduce types in many cases:
```lua
local a = sort(c, (a: Bar, b: Bar) -> a.x > b.x)
local b = filter(map(c, (a: number) -> a * 2), (a: number) -> a > 0)
```

Terse syntax for anonymous functions like this is used in various programming languages like C#, D, Java, JavaScript and Julia (ignoring `=>` vs `->`).

## Design

Syntax of *prefixexp* is extended with additional options:
Before:
```
prefixexp ::= NAME | `(' expr `)'
```
After:
```
prefixexp ::= NAME [`->' subexpr] | funcargs `->' subexpr | `(' expr `)'
```

Note that an anonymous function with a single argument is allowed to have a type annotation only when it's inside `()`.

We extend the syntax of *prefixexp* instead of *simpleexp* because we parse expressions inside parenthesis at this level.

### Parsing ``NAME [`->' subexpr]``

Parsing of the first option is easy, we check a single token for `->` after the NAME.

### Parsing ``funcargs `->' subexpr``

Parsing of the function argument list is in conflict with an expression in `` `(' expr `)'``.
To resolve this issue, we are going to perform the switch to potential function arguments after we consume the common `(` token.

At that point, if the next token is a `NAME`, we use a lookahead token to check if it's one of `:`, `,` or `)`.
In case it's `:` or `,` we proceed with function argument parsing.
In case it's `)` we will consume both `NAME` and `)` and try to figure out is we had a reference to a binding or if the next token is `->` or `:` in which case that's an anonymous function and we got one argument name.

---
When we are done with parsing, we re-use the existing AST for function expression, but the single expression that we have is placed inside AstStatReturn.

## Drawbacks

Support for the proposed syntax complicates parse of *prefixexp* and nears a boundary where we almost step into backtracing.

This feature also introduces an unfamiliar syntax of function definition to Luau for little real benefit.

## Alternatives

### Different token instead of `->`

Other programming languages use `=>` token, but in Luau it is not used right now.

### Introduction token

To avoid complications with parsing, we may use an introduction token and get rid of the `->` token.
One option might be the `do` token like in Ruby. Use of existing token preserves backwards compatibility:
```lua
local a = sort(c, do(a, b) a.x > b.x)
local b = filter(map(c, do(a) a * 2), do(a) a > 0)
```
```lua
local a = sort(c, do(a: Foo, b: Foo) a.x > b.x)
local b = filter(map(c, do(a: number) a * 2), do(a: number) a > 0)
```

Syntax of *simpleexp* is extended with a new option:
```
| `do` funcargs subexpr
```
