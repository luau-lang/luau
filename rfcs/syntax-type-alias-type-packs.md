# Type alias type packs

## Summary

Provide semantics for referencing type packs inside the body of a type alias declaration

## Motivation

We now have an ability to declare a placeholder for a type pack in type alias declaration, but there is no support to reference this pack inside the body of the alias:
```lua
type X<A...> = () -> A... -- cannot reference A... as the return value pack

type Y = X<number, string> -- invalid number of arguments
```

Additionally, while a simple introduction of these generic type packs into the scope will provide an ability to reference them in function declarations, we want to be able to use them to instantiate other type aliases as well.

Declaration syntax also supports multiple type packs, but we don't have defined semantics on instantiation of such alias.

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

Similar to function calls, we want to be able to assign multiple regular types to a single type pack:
```lua
type A = X                    -- T... = (). Note: check 'Alternatives'
type B = X<number>            -- T... = (number)
type C = X<number, string>    -- T... = (number, string)
```

Variadic types can also be assigned to type alias type pack:
```lua
type D = X<...number>         -- T... = (...number)
type E = X<number, ...string> -- T... = (number, ...string)
```

Multiple regular types can be assigned together with a type pack argument in a tail position:
```lua
type F<S...> = X<number, S...> -- T... = (number, S...)
type G<S...> = X<S..., number> -- error, type arguments can't follow type pack arguments
```

### Multiple type pack parameters

We have to keep in mind that it is also possible to declare a type alias that takes multiple type pack parameters.

And since we only allow type pack arguments after regular type arguments, trailing regular types with a type pack that follows are combined into a single type pack argument:
```lua
type Y<T..., U...> = --

type A<S...> = Y<S..., S...>              -- T... = (S...), U... = (S...)
type B<S...> = Y<number, ...string, S...> -- T... = (number, ...string), U... = S...
type C<S...> = Y<number, string, S...>    -- error, T... = (number, string, S...), but U... = undefined
type D = Y<...number>                     -- error, T = (...number), but U... = undefined, not (...number) even though one infinite set is enough to fill two, we may have '...number' inside a type pack argument and we'll be unable to see its content

type Z<T, U...> = --

type E<S...> = Z<number, S...>         -- T = number, U... = (S...)
type F<S...> = Z<number, string, S...> -- T = number, U... = (string, S...)
type G<S...> = Z<S...>                 -- error, not enough regular type arguments, can't split the front of S... into T

type W<T, U..., V...> = --

type H<S..., R...> = W<number, S..., R...>         -- U... = S..., V... = R...
type I<S..., R...> = W<number, string, S..., R...> -- U... = (string, S...), V... = R...
```

## Drawbacks

### Type pack element extraction

Because our type alias instantiations are not lazy, it's impossible to split of a single type from a type pack:
```lua
type Car<T, U...> = T

type X = Car<number, string, boolean> -- number
type Y<S...> = Car<S...>              -- error, not enough regular type arguments
type Z = Y<number, string, boolean>   -- error, Y doesn't have a valid definition
```

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

### Explicit type pack syntax

To enable additional control for the content of a type pack, we would have liked to introduce an explicit type pack syntax.

Similar to variadic types `...a` and generic type packs `T...`, explicit type packs can only be used at type pack positions:
```lua
type Y<T..., U...> = (T...) -> (U...)

type F1 = Y<(number, string), (boolean)>        -- T... = (number, string), U... = (boolean)
type F2 = Y<(), ()>                             -- T... = (), U... = ()
type F3<S...> = Y<string, S..., (number, S...)> -- T... = (string, S...), U... = (number, S...)
```

This explicit type pack syntax could've been used in other syntax positions where type packs are allowed:
```lua
local function f(...: (number, ...string)) end
local function g(...: (number, (string, number)) end
local function h(): (number, (string, ...boolean)) return 1, 's', true, false end
```

Unfortunately, our syntax supports placing parenthesis around the type:
```lua
local a: number
local b: (number)
```

This means that the syntax around type packs is ambiguous:
```lua
type Y<T..., U...> = (T...) -> (U...)

type A = Y<(number), (number)> -- will resolve as Y<number, number>

local function f(...: (number)) end -- '...' is a '...number', not a type pack with a single element
```

## Alternatives

The syntax we use right now to instantiate type alias with a single type pack parameter to an empty type pack is to not specify the argument list at all:
```lua
type X<T...> = --

type A = X     -- ok
type B = X<>   -- error
```

This is the current parser behavior and while we could introduce `X<>` as an alternative, without a warning/deprecation/epoch system we might not be able to disallow the `X` syntax.
