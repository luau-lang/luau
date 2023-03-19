# User-defined Type Predicates

## Summary

Support user-defined type predicates, which are runtime functions which a user can create and be used to refine the type
of a particular expression in the type checker.

## Motivation

The Luau type engine already provides built-in support for type predicates. In vanilla Luau, this can be seen
when using `typeof(X) == "type"`. Host environments can also specify their own predicate functions, such as
`Instance:IsA("ClassName")` in Roblox.

These predicate functions can then be used in combination with type guards or `assert(X)` statements (and, more recently, control flow analysis).
For example:

```lua
local x: unknown = ...
if typeof(x) == "string" then
    -- x is a string
    x ..= "value"
end
```

Right now, there is currently no way for a user to define their own predicate functions in code.
If a user defines their own type, it cannot be easily refined through the type checker, without using a type assertion
or just "assuming" the type is already correct. This is particularly prevelant in environments such as unsecure network code.

Consider the following function which is called by a uncontrolled client (i.e., any arguments can actually be passed into it):

```lua
type Pet = {
    name: string,
    age: number,
}

local function processClientEvent(pet: unknown)
    -- there is no way here to assert that pet is of type Pet
end
```

## Design

We propose a new syntax for return types in a function: `variable :: type`, where `variable` must match
the name of a function parameter. In context:

```lua
type Pet = {
    name: string,
    age: number,
}

local function isPet(x: unknown): x :: Pet
    return typeof(x) == "table" and typeof(x.name) == "string" and typeof(x.age) == "number"
end
```

Function type syntax can follow a similar structure:

```lua
type isPet = (x: unknown) -> x :: Pet
```

We can then use this in practice:

```lua
local function processClientEvent(pet: unknown)
    assert(isPet(pet), "invalid argument #1 to 'processClientEvent' (expected 'Pet')")
    -- here we now know that pet is of type Pet
    print(pet.name) -- no error
    print(pet.age) -- no error
    print(pet.size) -- TypeError
end
```

User defined type predicates can also be used in type guards, and control flow:

```lua
local function processClientEvent(pet: unknown)
    if isPet(pet) then
        -- pet is "Pet"
    end
    -- pet is "unknown"
end

local function processClientEvent(pet: unknown)
    if not isPet(pet) then return end
    -- pet is "Pet"
end
```

## Drawbacks

One problem with user-defined type predicates, is that they do not completely solve the unsoundness issue, if the user
incorrectly defines their type predicate function.

For example:

```lua
type Pet = {
    name: string,
    age: number,
    size: number,
}

local function isPet(x: unknown): x :: Pet
    -- bug: we have not verified that x contains a property "size"
    return typeof(x) == "table" and typeof(x.name) == "string" and typeof(x.age) == "number"
end

local function processClientEvent(pet: unknown)
    assert(isPet(pet))
    pet.size += 5 -- runtime error: attempt to perform arithmetic (add) on nil and number
end

processClientEvent({ name = "Cat", age = 5 })
```

## Alternatives

We could follow the syntax of [TypeScript type predicates](https://www.typescriptlang.org/docs/handbook/advanced-types.html):

```lua
local function isPet(x: unknown): x is Pet
    -- ...
end
```

This syntax could be feasible, with `is` being a context-sensitive keyword. We choose the type assertion operator `::`
to remain consistent with other syntax, and follow the idea that we are "asserting" the type of a parameter.

To fix the unsoundness drawback above, Luau could instead automatically generate type predicates for all custom defined user
types.
