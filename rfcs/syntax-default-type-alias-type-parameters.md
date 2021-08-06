# Default type alias type parameters

## Summary

Introduce syntax to provide default type values inside the type alias type parameter list.

## Motivation

Luau has support for type parameters for type aliases and functions.
In languages with similar features like C++ and TypeScript, it is possible to specify default values and users with experience in those languages would like to use that in Luau.

As an example, this feature can be used to define a equality comparator function type that by default accepts a single type argument and assigns the same type to the second argument, but it's still possible to specify both type arguments:
```lua
type Eq<T, U = T> = (l: T, r: U) -> boolean

local a: Eq<number> = ...
local b: Eq<number, string> = ...
```

While previous examples reference type parameters from the same list, it is also possible to specify concrete types:
```lua
type StrArray<T = string> = {arr: {T}, locked: boolean}

local a: StrArray = ...
local b: StrArray<MyString> = ...
```

Generic functions in Luau also have a type parameter list, but it's not possible to specify type arguments at the call site and because of that, default type argument values for generic functions are not proposed.

## Design

If a default type argument value is assigned, following type parameters (on the right) must also have default type argument values.
```lua
type A<T, U = string, V> = ... -- not allowed
```

Default type argument values can reference type parameters which were defined earlier (to the left):
```lua
type A<T, U = T> = ...-- ok

type A<T, U = V, V = T> = ... -- not allowed
```

Default type arguments are not allowed for type pack parameters.

---

Syntax for type alias type argument is extended as follows:

```typearg ::= Name [`=' typeannotation]```

Instead of storing a simple array of names in AstStatTypeAlias, we will store an array of structs containing the name and an optional default type value.

When type alias is referenced, missing type parameters are replaced with default type values, if they are available.

If all type parameters have a default type value, it is now possible to reference that without providing a type parameter list:
```lua
type All<T = string, U = number> = ...

local a: All -- ok
local b: All<> -- not allowed
```

If type is exported from a module, default type parameter values will still be available when module is imported.

## Drawbacks

Previously, it was possible to reference type alias without type arguments inside that alias (recursive types).
We disallow that now, but if we decide to return that, there will be an ambiguity if all type parameters have a default value.

## Alternatives

We could use `:` instead of `=` to define default type parameter values.
