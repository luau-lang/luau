# Lower Bounds Calculation

## Summary

We propose adapting lower bounds calculation from Pierce's Local Type Inference paper into the Luau type inference algorithm.

https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf

## Motivation

There are a number of important scenarios that occur where Luau cannot infer a sensible type without annotations.

Many of these revolve around type variables that occur in contravariant positions.

### Function Return Types

A very common thing to write in Luau is a function to try to find something in some data structure.  These functions habitually return the relevant datum when it is successfully found, or `nil` in the case that it cannot.  For instance:

```lua
function find_first_if(vec, f)
    for i, e in ipairs(vec) do
        if f(e) then
            return i
        end
    end

    return nil
end
```

This function has two `return` statements: One returns `number` and the other `nil`.  Today, Luau flags this as an error.  We ask authors to add a return annotation to make this error go away.

Higher order functions also present a problem.

```lua
function foo(f)
    f(5)
    f("string")
end
```

There is nothing wrong with the implementation of `foo` here, but Luau fails to typecheck it all the same because `f` is used in an inconsistent way.  This too can be worked around by introducing a type annotation for `f`.

## Design

We introduce a new kind of TypeVar, `ConstrainedTypeVar` to represent a TypeVar whose lower bounds are known.  We will never expose syntax for a user to write these types: They only temporarily exist as type inference is being performed.

When unifying some type with a `ConstrainedTypeVar` we _broaden_ the set of constraints that can be placed upon it.

It may help to realize that what we have been doing up until now has been _upper bounds calculation_.

When we `quantify` a function, we will _normalize_ each type and convert each `ConstrainedTypeVar` into a `UnionTypeVar`.

### Normalization

When computing lower bounds, we need to have some process by which we reduce types down to a minimal shape and canonicalize them, if only to have a clean way to flush out degenerate unions like `A | A`.

### Function subtyping relationships

If we are going to infer intersections of functions, then we need to be very careful about keeping combinatorics under control.  We therefore need to be very deliberate about what subtyping rules we have for functions of differing arity.  We have some important requirements:

* We'd like some way to canonicalize intersections of functions,
* optional function arguments are a great feature that we don't want to break, and

A very important use case for us is the case where the user is providing a callback to some higher-order function, and that function will be invoked with extra arguments that the original customer doesn't actually care about.  For example:

```lua
function map_array(arr, f)
    local result = {}
    for i, e in ipairs(arr) do
        table.insert(result, f(e, i, arr))
    end
    return result
end

local example = {1, 2, 3, 4}
local example_result = map_array(example, function(i) return i * 2 end)
```

This function mirrors the actual `Array.map` function in JavaScript.  It is very frequent for users of this function to provide a lambda that only accepts one argument.  It would be annoying for callers to be forced to provide a lambda that accepts two unused arguments.  This obviously becomes even worse if the function later changes to provide yet more optional information to the callback.

This use case is very important for Roblox, as we have many APIs that accept callbacks.  Implementors of those callbacks frequently omit arguments that they don't care about.

Here is an example straight out of the Roblox developer documentation. ([full example here](https://developer.roblox.com/en-us/api-reference/event/BasePart/Touched))

```lua
local part = script.Parent

local function blink()
    -- ...
end
    
part.Touched:Connect(blink)
```

The `Touched` event actually passes a single argument: the part that touched the Instance in question.  In this example, it is omitted from the callback handler.

We therefore want _oversaturation_ of a function to be allowed, but this is a double-edged sword.  Consider the following:

```lua
function double(x)
    return x * 2
end

function higher(f: () -> number)
    return f('five')
end

higher(double)
```

The problem we run into is

* If we allow the subtyping rule `(T) -> () <: () -> ()`, then normalizing the type `(T) -> () & () -> ()` will yield `() -> ()`, therefore
* We have no choice but to permit oversaturation of functions, therefore
* it is trivial to convert a `(T?) -> ()` into a `() -> ()` and to invoke that with a `U`, breaking soundness.

We need a solution here.

To resolve this, let's first examine the type of `double`.  It is tempting to type `double` as `(number) -> number` and call it a day, but, if we read the definition of the Lua language, we're being somewhat hasty when we do that.  We can pass _as many extra arguments as we want, of any type_ to this function.  We could therefore instead decide to type it as `(number, any...) -> number`.

So, we propose subtyping rules for type packs:

1. `(T0..TN) <: (U0..UN)` if, for each `T` and `U`, `T <: U`

1. `(T0..TN, A?) <: (T0..TN)` says that optional arguments can be left off by callers for type packs to make optional function arguments work.
1. `(T0..TN, any) <: (T0..TN)` just to be crystal clear that an argument whose type is `any` is also optional.
1. The type of any function literal whose arguments have fixed arity always have an implicit unnamed `any...` parameter.  It is safe to oversaturate such a function with any number of extra arguments of any type.
1. When calling a value whose type is a fixed-arity function, we permit it to be oversaturated with any number of `nil` values, and `nil` values _only_.

And the standard subtyping rule for functions

1. `T -> R <: U -> S` if `U <: T` and `R <: S`

Our `map_array` example typechecks because the lambda passed to `map_array` implicitly accepts `any...` in addition to its declared argument.

Our `higher` example does not typecheck because the function argument `f` can only be oversaturated with extra `nil` values.

## Drawbacks

## Alternatives
