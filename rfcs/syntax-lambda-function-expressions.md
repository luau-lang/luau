# Lambda function expressions 

## Summary

Introduce a way to define anonymous functions using a terse syntax.

## Motivation

Current function syntax takes up a lot of space when used for short anonymous that are passed in function calls.

By introducing a shorter syntax, we make code with heavy use of anonymous functions easier to write and arguably, easier to read as well.

Existing syntax:
```lua
local b = filter(map(c, function(a) return a * 2 end), function(a) return a > 0 end)
local b = filter(c, function(a) return a.good end)
local a = sort(c, function(a, b) return a.x > b.x end)
```

This proposal:
```lua
local b = filter(map(c, a -> a * 2), a -> a > 0)
local c = filter(c, a -> a.good)
local a = sort(c, do(a, b) -> a.x > b.x)
```

Type annotations are also supported, but with improvements to bi-directional type inference, typechecker should be able to deduce types in many cases:
```lua
local b = filter(map(c, do(a: number) -> a * 2), do(a: number) -> a > 0)
local c = filter(c, do(a): boolean -> a.good)
local a = sort(c, do(a: Bar, b: Bar) -> a.x > b.x)
```

## Design

Syntax of *prefixexp* is extended with an additional option:

Before:
```
prefixexp ::= NAME | `(' expr `)'
```
After:
```
prefixexp ::= NAME [`->' subexpr] | `(' expr `)'
```

Parsing of the this new option is easy, we check a single token for `->` after the NAME.

Syntax of *simpleexp* is extended with additional option:
```
| `do` `(' [parlist] `)' [`:' ReturnType] `->' subexpr
```

We use `do` as an introduction token here to keep the grammar simple without requiring look-ahead and backtracking.

While the `->` token here is not strictly required, it is more consistent with the single argument syntax and improves readability when return type annotations are used.

---
Note that these lambda function expressions support only a single return value using the *subexpr* grammar, compared to *explist* grammar that supports multiple comma-separated expressions.

When we are done with parsing, we re-use the existing AST for function expression, but the single *subexpr* that we have is placed inside AstStatReturn.

## Drawbacks

### Single return value
In Luau, multiple return value support is a first-class citizen, but the syntax becomes ambiguous if we allow that:
```lua
f(a, b -> b.a, b.b, c) -- how many values does the b return or the f accept?
``` 
As a solution, regular anonymous function definition can be used when multiple return values are required.

### A `function` with extra steps

This feature introduces an unfamiliar syntax of function definition for people coming from Lua and it adds little real benefit when we already have syntax for anonymous functions.

## Alternatives

### Drop the `do` introduction token

Original version of the proposal had the following syntax extension:

Before:
```
prefixexp ::= NAME | `(' expr `)'
```
After:
```
funchead ::= `(' [parlist] `)' [`:' ReturnType]
prefixexp ::= NAME [`->' subexpr] | funchead `->' subexpr | `(' expr `)'
```

Terse syntax for anonymous functions like this is used in various programming languages like C#, D, Java, JavaScript and Julia (ignoring `=>` vs `->`).

The problem with this option is that parsing with limited look-ahead and without backtracing becomes very complicated, and unavoidable in cases like:
```lua
f((x):number(2))
```
Only after the return type annotation can we see that there is no `->` token and this was a member function call.
