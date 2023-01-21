# Constrained Generics

## Summary

Allows you to give a function generic a constrained type.

## Motivation

Right now, when you create a generic function, the generic paramter is assumed to be everything. This creates problems, especially with tables
```lua
local function SomethingWithAGenericTable<T>(tab: T)
  for _, v in t do --Cannot call non-function T (T is not a table)

  end
end
```

This constraining is mainly for concrete table types and functions, since types like `number`, `string`, and `boolean` are literal types.

## Design

The way to implement this is to have new syntax under the generic's brackets: `<T: typedef>`. This would constrain the type from the generic def and give the typechecker better hints.
```lua
local function SomethingWithAGenericTable<T: {[any]: any}>(tab: T)
  for _, v in t do
    print(v)
  end
end

SomethingWithAGenericTable({meow = 5}) --OK
SomethingWithAGenericTable(5) --not OK
```

## Drawbacks

This adds unusual syntax, as well as adding to the complexity of the language.

This also doesn't address how this should be approached around type packs.

## Alternatives

Use something similar to Rust's `where` syntax:
```lua
local function SomethingWithAGenericTable<T>(tab: T) where T: {[string]: any}
```

Do nothing, and use the `assert(type(T) == T)` syntax, though this creates extra bytecode. It also casts the object as `never`, which removes the type errors, however no autocomplete information is generated.
