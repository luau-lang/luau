# nil-forgiving postfix operator !

## Summary

Introduce syntax to suppress typechecking errors for nilable types by ascribing them into a non-nil type.

## Motivation

Typechecking might not be able to figure out that a certain expression is a non-nil type, but the user might know additional context of the expression.

Using `::` ascriptions is the current work-around for this issue, but it's much more verbose and requires providing the full type name when you only want to ascribe T? to T.

The nil-forgiving operator will also allow chaining to be written in a very terse manner:
```lua
local p = a!.b!.c
```
instead of
```lua
local ((p :: Part).b :: Folder).c
```

Note that nil-forgiving operator is **not** a part of member access operator, it can be used in standalone expressions, indexing and other places:
```lua
local p = f(a!)!
local q = b!['X']
```

Nil-forgiving operator (also known as null-forgiving or null-suppression operator) can be found in some programming languages such as C# and TypeScript.

## Design

To implement this, we will change the syntax of the *primaryexp*.

Before:
```
primaryexp ::= prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs }
```
After:
```
postfixeexp ::= (`.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs) [`!']
primaryexp ::= prefixexp [`!'] { postfixeexp }
```

When we get the `!` token, we will wrap the expression that we have into a new AstExprNonNilAssertion node.

This operator doesn't have any impact on the run-time behavior of the program, it will only change the type of the expression in the typechecker.

---
If the operator is used on expression of type that is already non-nil, it has no effect and doesn't generate additional warnings.

The reason for this is to simplify movement of existing code where context in each location is slightly different.

As an example from Roblox, instance path could dynamically change from being know to exist to be missing when script is changed in edit mode.

## Drawbacks

It might be useful to warn about unnecessary uses of this operator, but we have no way way of enabling this behavior.

It may be possible to warn for some limited number of cases, for example, when used on l-value:
```lua
p.a! = b
```
Another case is for constants, but the check will require an AST match:
```lua
local a = 2!
local b = "str"!
```
