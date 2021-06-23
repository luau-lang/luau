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

Nil-forgiving operator can be found in some programming languages such as C# (null-forgiving or null-suppression operator) and TypeScript (non-null assertion operator).

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

An error is generated when the type of expression node this is one of the following:
* AstExprConstantBool
* AstExprConstantNumber
* AstExprConstantString
* AstExprFunction
* AstExprTable

This operator doesn't have any impact on the run-time behavior of the program, it will only affect the type of the expression in the typechecker.

---
While parsing an assignment expression starts with a *primaryexp*, it performs a check that it has an l-value based on a fixed set of AstNode types.

Since using `!` on an l-value has no effect, we don't extend this list with the new node and will generate a specialized parse error for code like:
```lua
p.a! = b
```

---
When operator is used on expression of a union type with a `nil` option, it removes that option from the set.
If only one option remains, the union type is replaced with the type of a single option.

If the type is `nil`, typechecker will generate a warning that the operator cannot be applied to the expression.

For any other type, it has no effect and doesn't generate additional warnings.

The reason for the last rule is to simplify movement of existing code where context in each location is slightly different.

As an example from Roblox, instance path could dynamically change from being know to exist to be missing when script is changed in edit mode.

## Drawbacks

### Unnecessary operator use

It might be useful to warn about unnecessary uses of this operator when the value cannot be `nil`, but we have no way of enabling this behavior.

### Bad practice

The operator might be placed by users to ignore/silence correct warnings and lower the strength of type checking that Luau provides.

## Alternatives

Aside from type assertion operator :: it should be possible to place `assert` function calls before the operation.
Type refinement/constraints should handle that statement and avoid warning in the following expressions.

But `assert` call will introduce runtime overhead without adding extra safety to the case when the type is nil at run time, in both cases an error will be thrown.
