## Summary

Introduce a form of list comprehension using `for var in iterator do` syntax.

## Motivation

List comprehensions would bring several benefits and prevent code smell.
In Lua you are encouraged to not modify tables during traversal.

When traversing a table to exclude all the odd numbers you'd be creating a large statement to get rid of them

## Design

To solve these problems, I propose a `n for-in-do` expression form that is syntactically similar to a for statement, but lacks terminating `end`.
