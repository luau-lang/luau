## Summary

Introduce a form of list comprehension using `n for-in-do` syntax.

## Motivation

List comprehensions would bring several benefits and prevent code smell.
In Lua you are encouraged to not modify tables during traversal.

When traversing a table to exclude all the odd numbers you'd be creating a large statement to get rid of them

## Design

To solve these problems, I propose a `n for-in-do` expression form that is syntactically similar to a for statement, but lacks terminating `end`.

The `n for-in-do` expression must match ``<identifier> for <identifier> in <expr> do``

## Drawbacks

List comprehensions may be misused.

The list comprehensions have similar drawbacks to the if expression drawbacks
https://github.com/Roblox/luau/blob/master/rfcs/syntax-if-expression.md

## Alternatives

### C#
```csharp
var list2 = from number in Enumerable.Range(0, 10) select 2*number;
```

### Rust (using cute)
```rust
let vector = c![x, for x in 1..10, if x % 2 == 0];
```

### Haskell
```haskell
[x * 2 | x <- [0 .. 99], x * x > 3]
```

### R
```r
 x <- 0:100
 S <- 2 * x[x ^ 2 > 3]
 ```

### Function syntax
List comprehensions could be implemented as functions like ``table.map`` or ``table.filter`` 
