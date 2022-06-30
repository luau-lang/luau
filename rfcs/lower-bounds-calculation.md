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
-- A.lua
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

We would like to automatically infer `find_first_if : <T>({T}, (T) -> boolean) -> number?`.

Higher order functions also present a similar problem.

```lua
-- B.lua
function foo(f)
    f(5)
    f("string")
end
```

There is nothing wrong with the implementation of `foo` here, but Luau fails to typecheck it all the same because `f` is used in an inconsistent way.  This too can be worked around by introducing a type annotation for `f`.

The fact that the return type of `f` is never used confounds things a little, but for now it would be a big improvement if we inferred `f : <T...>((number | string) -> T...) -> ()`.

## Design

We introduce a new kind of TypeVar, `ConstrainedTypeVar` to represent a TypeVar whose lower bounds are known.  We will never expose syntax for a user to write these types: They only temporarily exist as type inference is being performed.

When unifying some type with a `ConstrainedTypeVar` we _broaden_ the set of constraints that can be placed upon it.

It may help to realize that what we have been doing up until now has been _upper bounds calculation_.

When we `quantify` a function, we will _normalize_ each type and convert each `ConstrainedTypeVar` into a `UnionTypeVar`.

### Normalization

When computing lower bounds, we need to have some process by which we reduce types down to a minimal shape and canonicalize them, if only to have a clean way to flush out degenerate unions like `A | A`.  Normalization is about reducing union and intersection types to a minimal, canonicalizable shape.

A normalized union is one where there do not exist two branches on the union where one is a subtype of the other.  It is quite straightforward to implement.

A normalized intersection is a little bit more complicated:

1. The tables of an intersection are always combined into a single table.  Coincident properties are merged into intersections of their own.
    * eg `normalize({x: number, y: string} & {y: number, z: number}) == {x: number, y: string & number, z: number}`
    * This is recursive.  eg `normalize({x: {y: number}} & {x: {y: string}}) == {x: {y: number & string}}`
1. If two functions in the intersection have a subtyping relationship, the normalization results only in the super-type-most function. (more on function subtyping later)

### Function subtyping relationships

If we are going to infer intersections of functions, then we need to be very careful about keeping combinatorics under control.  We therefore need to be very deliberate about what subtyping rules we have for functions of differing arity.  We have some important requirements:

* We'd like some way to canonicalize intersections of functions, and yet
* optional function arguments are a great feature that we don't want to break

A very important use case for us is the case where the user is providing a callback to some higher-order function, and that function will be invoked with extra arguments that the original customer doesn't actually care about.  For example:

```lua
-- C.lua
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
-- D.lua
local part = script.Parent

local function blink()
    -- ...
end

part.Touched:Connect(blink)
```

The `Touched` event actually passes a single argument: the part that touched the `Instance` in question.  In this example, it is omitted from the callback handler.

We therefore want _oversaturation_ of a function to be allowed, but this combines with optional function arguments to create a problem with soundness.  Consider the following:

```lua
-- E.lua
type Callback = (Instance) -> ()

local cb: Callback
function register_callback(c: Callback)
    cb = c
end

function invoke_callback(i: Instance)
    cb(i)
end

---

function bad_callback(x: number?)
end

local obscured: () -> () = bad_callback

register_callback(obscured)

function good_callback()
end

register_callback(good_callback)
```

The problem we run into is, if we allow the subtyping rule `(T?) -> () <: () -> ()` and also allow oversaturation of a function, it becomes easy to obscure an argument type and pass the wrong type of value to it.

Next, consider the following type alias

```lua
-- F.lua
type OldFunctionType = (any, any) -> any
type NewFunctionType = (any) -> any
type FunctionType = OldFunctionType & NewFunctionType
```

If we have a subtyping rule `(T0..TN) <: (T0..TN-1)` to permit the function subtyping relationship `(T0..TN-1) -> R <: (T0..TN) -> R`, then the above type alias normalizes to `(any) -> any`.  In order to call the two-argument variation, we would need to permit oversaturation, which runs afoul of the soundness hole from the previous example.

We need a solution here.

To resolve this, let's reframe things in simpler terms:

If there is never a subtyping relationship between packs of different length, then we don't have any soundness issues, but we find ourselves unable to register `good_callback`.

To resolve _that_, consider that we are in truth being a bit hasty when we say `good_callback : () -> ()`.  We can pass any number of arguments to this function safely.  We could choose to type `good_callback : () -> () & (any) -> () & (any, any) -> () & ...`.  Luau already has syntax for this particular sort of infinite intersection: `good_callback : (any...) -> ()`.

So, we propose some different inference rules for functions:

1. The AST fragment `function(arg0..argN) ... end` is typed `(T0..TN, any...) -> R` where `arg0..argN : T0..TN` and `R` is the inferred return type of the function body.  Function statements are inferred the same way.
1. Type annotations are unchanged.  `() -> ()` is still a nullary function.

For reference, the subtyping rules for unions and functions are unchanged.  We include them here for clarity.

1. `A <: A | B`
1. `B <: A | B`
1. `A | B <: T` if `A <: T` or `B <: T`
1. `T -> R <: U -> S` if `U <: T` and `R <: S`

We propose new subtyping rules for type packs:

1. `(T0..TN) <: (U0..UN)` if, for each `T` and `U`, `T <: U`
1. `(U...)` is the same as `() | (U) | (U, U) | (U, U, U) | ...`, therefore
1. `(T0..TN) <: (U...)` if for each `T`, `T <: U`, therefore
1. `(U...) -> R <: (T0..TN) -> R` if for each `T`, `T <: U`

The important difference is that we remove all subtyping rules that mention options.  Functions of different arities are no longer considered subtypes of one another.  Optional function arguments are still allowed, but function as a feature of function calls.

Under these rules, functions of different arities can never be converted to one another, but actual functions are known to be safe to oversaturate with anything, and so gain a type that says so.

Under these subtyping rules, snippets `C.lua` and `D.lua`, check the way we want: literal functions are implicitly safe to oversaturate, so it is fine to cast them as the necessary callback function type.

`E.lua` also typechecks the way we need it to: `(Instance) -> () </: () -> ()` and so `obscured` cannot receive the value `bad_callback`, which prevents it from being passed to `register_callback`.  However, `good_callback : (any...) -> ()` and `(any...) -> () <: (Instance) -> ()` and so it is safe to register `good_callback`.

Snippet `F.lua` is also fixed with this ruleset: There is no subtyping relationship between `(any) -> ()` and `(any, any) -> ()`, so the intersection is not combined under normalization.

This works, but itself creates some small problems that we need to resolve:

First, the `...` symbol still needs to be unavailable for functions that have been given this implicit `...any` type.  This is actually taken care of in the Luau parser, so no code change is required.

Secondly, we do not want to silently allow oversaturation of direct calls to a function if we know that the arguments will be ignored.  We need to treat these variadic packs differently when unifying for function calls.

Thirdly, we don't want to display this variadic in the signature if the author doesn't expect to see it.

We solve these issues by adding a property `bool VariadicTypePack::hidden` to the implementation and switching on it in the above scenarios.  The implementation is relatively straightforward for all 3 cases.

## Drawbacks

There is a potential cause for concern that we will be inferring unions of functions in cases where we previously did not.  Unions are known to be potential sources of performance issues.  One possibility is to allow Luau to be less intelligent and have it "give up" and produce less precise types.  This would come at the cost of accuracy and soundness.

If we allow functions to be oversaturated, we are going to miss out on opportunities to warn the user about legitimate problems with their program.  I think we will have to work out some kind of special logic to detect when we are oversaturating a function whose exact definition is known and warn on that.

Allowing indirect function calls to be oversaturated with `nil` values only should be safe, but a little bit unfortunate.  As long as we statically know for certain that `nil` is actually a permissible value for that argument position, it should be safe.

## Alternatives

If we are willing to sacrifice soundness, we could adopt success typing and come up with an inference algorithm that produces less precise type information.

We could also technically choose to do nothing, but this has some unpalatable consequences:  Something I would like to do in the near future is to have the inference algorithm assume the same `self` type for all methods of a table.  This will make inference of common OO patterns dramatically more intuitive and ergonomic, but inference of polymorphic methods requires some kind of lower bounds calculation to work correctly.
