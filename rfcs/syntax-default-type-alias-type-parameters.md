# Default type alias type parameters

## Summary

Introduce syntax to provide default type values inside the type alias type parameter list.

## Motivation

Luau has support for type parameters for type aliases and functions.
In languages with similar features like C++, Rust, Flow and TypeScript, it is possible to specify default values for looser coupling and easier composability, and users with experience in those languages would like to have these design capabilities in Luau.

Here is an example that is coming up frequently during development of GraphQL Luau library:
```lua
export type GraphQLFieldResolver<
    TSource,
    TContext,
    TArgs = { [string]: any }
> = (TSource, TArgs, TContext, GraphQLResolveInfo) -> any
```
If we could specify defaults like that, we won't have to write long type names when type alias is used unless specific customization is required.
Some engineers already skip these extra arguments and use `'any'` to save time, which gives worse typechecking quality.

Without default parameter values it's also harder to refactor the code as each type alias reference that uses 'common' type arguments has to be updated.

While previous example uses a concrete type for default type value, it should also be possible to reference generic types from the same list:
```lua
type Eq<T, U = T> = (l: T, r: U) -> boolean

local a: Eq<number> = ...
local b: Eq<number, string> = ...
```

Generic functions in Luau also have a type parameter list, but it's not possible to specify type arguments at the call site and because of that, default type parameter values for generic functions are not proposed.

## Design

If a default type parameter value is assigned, following type parameters (on the right) must also have default type parameter values.
```lua
type A<T, U = string, V> = ... -- not allowed
```

Default type parameter values can reference type parameters which were defined earlier (to the left):
```lua
type A<T, U = T> = ...-- ok

type A<T, U = V, V = T> = ... -- not allowed
```

Default type parameter values are also allowed for type packs:
```lua
type A<T, U... = ...string>           -- ok, variadic type pack
type B<T, U... = ()>                  -- ok, type pack with no elements
type C<T, U... = (string)>            -- ok, type pack with one element
type D<T, U... = (string, number)>    -- ok, type pack with two elements
type E<T, U... = (string, ...number)> -- ok, variadic type pack with a different first element
type F<T..., U... = T...>             -- ok, same type pack as T...
```

---

Syntax for type alias type parameter is extended as follows:

```typeparameter ::= Name [`...'] [`=' typeannotation]```

Instead of storing a simple array of names in AstStatTypeAlias, we will store an array of structs containing the name and an optional default type value.

When type alias is referenced, missing type parameters are replaced with default type values, if they are available.

If all type parameters have a default type value, it is now possible to reference that without providing a type parameter list:
```lua
type All<T = string, U = number> = ...

local a: All -- ok
local b: All<> -- ok as well
```

If type is exported from a module, default type parameter values will still be available when module is imported.

---
Type annotations in Luau are placed after `':'`, but we use `'='` here to assign a type value, not to imply that the type parameter on the left has a certain type.

Type annotation with `':'` could be used in the future for bounded quantification which is orthogonal to the default type value.

## Drawbacks

Other languages might allow references to the type alias without arguments inside the scope of that type alias to resolve into a recursive reference to the type alias with the same arguments.

While that is not allowed in Luau right now, if we decide to change that in the future, we will have an ambiguity when all type alias parameters have default values:
```lua
-- ok if we allow Type to mean Type<A, B>
type Type<A, B> = { x: number, b: Type? }

-- ambiguity, Type could mean Type<number, string> or Type<A, B>
type Type<A = number, B = string> = { x: number, b: Type? }
```
