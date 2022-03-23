# if-then-else expression

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Introduce a form of ternary conditional using `if cond then value else alternative` syntax.

## Motivation

Luau does not have a first-class ternary operator; when a ternary operator is needed, it is usually emulated with `and/or` expression, such as `cond and value or alternative`.

This expression evaluates to `value` if `cond` and `value` are truthy, and `alternative` otherwise. In particular it means that when `value` is `false` or `nil`, the result of the entire expression is `alternative` even when `cond` is truthy - which doesn't match the expected ternary logic and is a frequent source of subtle errors.

Instead of `and/or`, `if/else` statement can be used but since that requires a separate mutable variable, this option isn't ergonomic. An immediately invoked function expression is also unergonomic and results in performance issues at runtime.

## Design

To solve these problems, we propose introducing a first-class ternary conditional. Instead of `? :` common in C-like languages, we propose an `if-then-else` expression form that is syntactically similar to `if-then-else` statement, but lacks terminating `end`.

Concretely, the `if-then-else` expression must match `if <expr> then <expr> else <expr>`; it can also contain an arbitrary number of `elseif` clauses, like `if <expr> then <expr> elseif <expr> then <expr> else <expr>`. Unlike if statements, `else` is mandatory.

The result of the expression is the then-expression when condition is truthy (not `nil` or `false`) and else-expression otherwise. Only one of the two possible resulting expressions is evaluated.

Example:

```lua
local x = if FFlagFoo then A else B

MyComponent.validateProps = t.strictInterface({
    layoutOrder = t.optional(t.number),
    newThing = if FFlagUseNewThing then t.whatever() else nil,
})
```

Note that `else` is mandatory because it's always better to be explicit. If it weren't mandatory, it opens the possiblity that someone might be writing a chain of if-then-else and forgot to add in the final `else` that _doesn't_ return a `nil` value! Enforcing this syntactically ensures the program does not run. Also, with it being mandatory, it solves many cases where parsing the expression is ambiguous due to the infamous [dangling else](https://en.wikipedia.org/wiki/Dangling_else).

This example will not do what it looks like it's supposed to do! The if expression will _successfully_ parse and be interpreted as to return `h()` if `g()` evaluates to some falsy value, when in actual fact the clear intention is to evaluate `h()` only if `f()` is falsy.

```lua
if f() then
    ...
    local foo = if g() then x
else
    h()
    ...
end
```

The only way to solve this had we chose optional `else` branch would be to wrap the if expression in parentheses or to place a semi-colon. 

## Drawbacks

Studio's script editor autocomplete currently adds an indented block followed by `end` whenever a line ends that includes a `then` token. This can make use of the if expression unpleasant as developers have to keep fixing the code by removing auto-inserted `end`. We can work around this on the editor side by (short-term) differentiating between whether `if` token is the first on its line, and (long-term) by refactoring completion engine to use infallible parser for the block completer.

Parser recovery can also be more fragile due to leading `if` keyword - when `if` was encountered previously, it always meant an unfinished expression, but now it may start an `if-expr` that, when confused with `if-end` statement can lead to a substantially incorrect parse that is difficult to recover from. However, similar issues occur frequently due to function call statements and as such it's not clear that this makes the recovery materially worse.

While this is not a problem today, in the past we've contemplated adding support for mid-block `return` statements; these would create an odd grammatical quirk where an `if..then` statement following an empty `return` would parse as an `if` expression. This would happen even without `if` expressions though for function calls (e.g. `return` followed by `print(1)`), and is more of a problem with the potential `return` statement changes and less of a problem with this proposal.

## Alternatives

We've evaluated many alternatives for the proposed syntax.

### Python syntax
```
b if a else c
```
Undesirable because expression evaluation order is not left-to-right which is a departure from all other Lua expressions. Additionally, since `b` may be ending a statement (followed by `if` statement), resolving this ambiguity requires parsing `a` as expression and backtracking if `else` is not found, which is expensive and likely to introduce further ambiguities.

### C-style ternary operator
```
a ? b : c
```
Problematic because `:` is used for method calls. In Julia `? :` and `:` are both operators which are disambiguated by _requiring_ spaces in the first case and _prohibiting_ them in the second case; this breaks backwards compatibility and doesn't match the rest of the language where whitespace in the syntax is not significant.

### Function syntax
```
iff(a, b, c)
```
If implemented as a regular function call, this would break short-circuit behavior. If implemented as a special builtin, it would look like a regular function call but have magical behavior -- something likely to confuse developers.

### Perl 6 syntax
```
a ?? b !! c
```
Syntax deemed too unconventional to use in Luau.

### Smaller variations
```
(if a then b else c)
```
Ada uses this syntax (with parentheses required for clarity). Similar solutions were discussed for `as` previously and rejected to make it easier for humans and machines to understand the language syntax.

```
a then b else c
```
This is ambiguous in some cases (like within if condition) so not feasible from a grammar perspective.

```
if a then b else c end
```
The `end` here is unnecessary since `c` is not a block of statements -- it is simply an expression. Thus, use of `end` here would be inconsistent with its other uses in the language. It also makes the syntax more cumbersome to use and could lead to developers sticking with the error-prone `a and b or c` alternative.

### `elseif` support

We discussed a simpler version of this proposal without `elseif` support. Unlike if statements, here `elseif` is purely syntactic sugar as it's fully equivalent to `else if`. However, supporting `elseif` makes if expression more consistent with if statement - it is likely that developers familiar with Luau are going to try using `elseif` out of habit. Since supporting `elseif` here is trivial we decided to keep it for consistency.
