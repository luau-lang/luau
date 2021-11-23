# Type alias type packs

**Status**: Implemented

## Summary

Provide semantics for referencing type packs inside the body of a type alias declaration

## Motivation

We now have an ability to declare a placeholder for a type pack in type alias declaration, but there is no support to reference this pack inside the body of the alias:
```lua
type X<A...> = () -> A... -- cannot reference A... as the return value pack

type Y = X<number, string> -- invalid number of arguments
```

Additionally, while a simple introduction of these generic type packs into the scope will provide an ability to reference them in function declarations, we want to be able to use them to instantiate other type aliases as well.

Declaration syntax also supports multiple type packs, but we don't have defined semantics on instantiation of such type alias.

## Design

We currently support type packs at these locations:
```lua
-- for variadic function parameter when type pack is generic
local function f<a...>(...: a...)

-- for multiple return values
local function f<a...>(): a...

-- as the tail item of function return value pack
local function f<a...>(): (number, a...)
```

We want to be able to use type packs for type alias instantiation:
```lua
type X<T...> = --

type A<S...> = X<S...> -- T... = (S...)
```

Similar to function calls, we want to be able to assign zero or more regular types to a single type pack:
```lua
type A = X<>                  -- T... = ()
type B = X<number>            -- T... = (number)
type C = X<number, string>    -- T... = (number, string)
```

Definition of `A` doesn't parse right now, we would like to make it legal going forward.

Variadic types can also be assigned to type alias type pack:
```lua
type D = X<...number>           -- T... = (...number)
```

### Multiple type pack parameters

We have to keep in mind that it is also possible to declare a type alias that takes multiple type pack parameters.

Again, type parameters that haven't been matched with type arguments are combined together into the first type pack.
After the first type pack parameter was assigned, following type parameters are not allowed.
Type pack parameters after the first one have to be type packs:
```lua
type Y<T..., U...> = --

type A<S...> = Y<S..., S...>              -- T... = S..., U... = S...
type B<S...> = Y<...string, S...>         -- T... = (...string), U... = S...
type C<S...> = Y<number, string, S...>    -- T... = (number, string), U... = S...
type D = Y<...number>                     -- error, T = (...number), but U... = undefined, not (...number) even though one infinite set is enough to fill two, we may have '...number' inside a type pack argument and we'll be unable to see its content
type E<S...> = Y<S..., number, string>    -- error, type parameters are not allowed after a type pack

type Z<T, U...> = --

type F<S...> = Z<number, S...>         -- T = number, U... = S...
type G<S...> = Z<S...>                 -- error, not enough regular type arguments, can't split the front of S... into T

type W<T, U..., V...> = --

type H<S..., R...> = W<number, S..., R...>  -- U... = S..., V... = R...
type I<S...> = W<number, string, S...>      -- U... = (string), V... = S...
```

### Explicit type pack syntax

To enable additional control for the content of a type pack, especially in cases where multiple type pack parameters are expected, we introduce an explicit type pack syntax for use in type alias instantiation.

Similar to variadic types `...a` and generic type packs `T...`, explicit type packs can only be used at type pack positions:
```lua
type Y<T..., U...> = (T...) -> (U...)

type F1 = Y<(number, string), (boolean)>          -- T... = (number, string), U... = (boolean)
type F2 = Y<(), ()>                               -- T... = (), U... = ()
type F3<S...> = Y<string, number, (number, S...)> -- T... = (string, number), U... = (number, S...)
```

In type parameter list, types inside the parentheses always produce a type pack.
This is in contrast to function return type pack annotation, where `() -> number` is the same as `() -> (number)`.

However, to preserve backwards-compatibility with optional parenthesis around regular types, type alias instantiation is allowed to assign a non-variadic type pack parameter with a single element to a type argument:
```lua
type X<T, U> = (T) -> U?
type A = X<(number), (string)> -- T = number, U = string
type A = X<(number), string>   -- same

type Y<T...> = (T...) -> ()
type B = Y<(number), (string)> -- error: too many type pack parameters
```

Explicit type pack syntax is not available in other type pack annotation contexts.

## Drawbacks

### Type pack element extraction

Because our type alias instantiations are not lazy, it's impossible to split of a single type from a type pack:
```lua
type Car<T, U...> = T

type X = Car<number, string, boolean> -- number
type Y<S...> = Car<S...>              -- error, not enough regular type arguments
type Z = Y<number, string, boolean>   -- error, Y doesn't have a valid definition
```

With our immediate instantiation, at the point of `Car<S...>`, we only know that `S...` is a type pack, but contents are not known.

Splitting off a single type is is a common pattern with variadic templates in C++, but we don't allow type alias overloads, so use cases are more limited.

### Type alias can't result in a type pack

We don't propose type aliases to generate type packs, which could have looked as:
```lua
type Car<T, U...> = T
type Cdr<T, U...> = U...
type Cons<T, U...> = (T, U...)

--[[
 using type functions to operate on type packs as a list of types
]]
```

We wouldn't be able to differentiate if an instantiation results in a type or a type pack and our type system only allows variadic types as the type pack tail element.

Support for variadic types in the middle of a type pack can be found in TypeScript's tuples.

## Alternatives

### Function return type syntax for explicit type packs

Another option that was considered is to parse `(T)` as `T`, like we do for return type annotation.

This option complicates the match ruleset since the typechecker will never know if the user has written `T` or `(T)` so each regular type could be a single element type pack and vice versa.
```lua
type X<T...>
type C = X<number, number> -- T... = (number, number)
type D = X<(number), (number)> -- T... = (number, number)

type Y<T..., U...>

--- two items that were enough to satisfy only a single T... in X are enough to satisfy two T..., U... in Y
type E = Y<number, number> -- T... = (number), U... = (number)
```

### Special mark for single type type packs

In the Rust language, there is a special disambiguation syntax for single element tuples and single element type packs using a trailing comma:
```rust
(Type,)
```

In Python, the same idea is used for single element tuple values:
```python
value = (1, )
```

Since our current ruleset no longer has a problem with single element type tuples, I don't think we need syntax-directed disambiguation option like this one.

### Only type pack arguments for type pack parameters

One option that we have is to remove implicit pack assignment from a set of types and always require new explicit type pack syntax:

```lua
type X<T...> = --

type B = X<>                  -- invalid
type C = X<number>            -- invalid
type D = X<number, string>    -- invalid

type B = X<()>                -- T... = ()
type C = X<(number)>          -- T... = (number)
type D = X<(number, string)>  -- T... = (number, string)
```

But this doesn't allow users to define type aliases where they only care about a few types and use the rest as a 'tail':

```lua
type X<T, U, Rest...> = (T, U, Rest...) -> Rest...

type A = X<number, string, ()> -- forced to use a type pack when there are no tail elements
```

It also makes it harder to change the type parameter count without fixing up the instantiations.

### Combining types together with the following type pack into a single argument

Earlier version of the proposal allowed types to be combined together with a type pack as a tail:
```lua
type X<T...> = --

type A<S...> = X<number, S...> --- T... = (number, S...)
```

But this syntax resulted in some confusing behavior when multiple type pack arguments are expected:
```lua
type Y<T..., U...> = --

type B = Y<number, (string, number)> -- not enough type pack parameters
```
