# Compound assignment using `op=` syntax

> Note: this RFC was adapted from an internal proposal that predates RFC process and as such doesn't follow the template precisely

**Status**: Implemented

## Design

A feature present in many many programming languages is assignment operators that perform operations on the left hand side, for example

```
a += b
```

Lua doesn't provide this right now, so it requires code that's more verbose, for example

```
data[index].cost = data[index].cost + 1
```

This proposal suggests adding `+=`, `-=`, `*=`, `/=`, `%=`, `^=` and `..=` operators to remedy this. This improves the ergonomics of writing code, and occasionally results in code that is easier to read to also be faster to execute.

The semantics of the operators is going to be as follows:

- Only one value can be on the left and right hand side
- The left hand side is evaluated once as an l-value, similarly to the left hand side of an assignment operator
- The right hand side is evaluated as an r-value (which results in a single Lua value)
- The assignment-modification is performed, which can involve table access if the left hand side is a table dereference
- Unlike C++, these are *assignment statements*, not expressions - code like this `a = (b += 1)` is invalid.

Crucially, this proposal does *not* introduce new metamethods, and instead uses the existing metamethods and table access semantics, for example

```
data[index].cost += 1
```

translates to

```
local table = data[index]
local key = "cost"
table[key] = table[key] + 1
```

Which can invoke `__index` and `__newindex` on table as necessary, as well as `__add` on the element. In this specific example, this is *faster* than `data[index].cost = data[index].cost + 1` because `data[index]` is only evaluated once, but in general the compound assignment is expected to have the same performance and the goal of this proposal is to make code easier and more pleasant to write.

The proposed new operators are currently invalid in Lua source, and as such this is a backwards compatible change.

From the implementation perspective, this requires adding new code/structure to AST but doesn't involve adding new opcodes, metatables, or any extra cost at runtime.
