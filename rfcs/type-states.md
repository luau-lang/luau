# Type states

## Summary

Track the most precise types from the assignments to any lvalues.

## Motivation

Today, when we check this trivial program, we report a false positive `Value of type '{| x: number |}?' could be nil`.

```lua
local t: {x: number}? = {x = 5}
print(t.x) -- right here
```

But the program is clearly well formed. The lvalue `t` is initialized with a table. Alas, Luau doesn't know this because there's nothing indicating the current state of `t`. As a result whenever we refer to `t` in `Scope::bindings`, we have no choice but to look at the type of `t` which _functionally acts_ like a subtype constraint.

There are some fallout from not doing this. For example

1. many false positive type errors, negatively impacting the UX and value of each type errors
2. in some no errors are reported when there ought to be one, negatively impacting the value of type errors

The solution is blatant: we need to start tracking the types assigned to each lvalues.

## Design

A good chunk of the groundwork is already in place for this, `RefinementMap`. All we'd need to do is build on top of that existing system. It's going to be renamed into `TypeState`, and it will instead keep a record of every assignments and refinements, order and method preserving.

That is, `local t: {x: number}? = {x = 5}` should add the lvalue `t` with `{x: number}` as the current state. And that's it, it's enough to know that `print(t.x)` is well formed because `t.x` is not going to crash the program. Because `t` was _annotated_ with `{x: number}?`, it's ok to write `t = nil` afterwards, all we'll do is update the record saying `t` is now `nil`. So the above program will now type check with no type errors.

Let's start by covering some of the basic cases:

```lua
local x: string | number | nil = nil

if math.random() > 0.5 then
    x = "hello"
else
    x = 5
end

local y: string | number = x
```

Here's another one:

```lua
local x: string | number = "hi"

local a: string = x

x = 5

local b: number = x
```

And another:

```lua
local x, y = "hi", 5

x, y = y, x

local a: number, b: string = x, y
```

Not too interesting. Let's kick things up a notch.

### Type states does not care about type errors.

Even if type errors occurs, type states is going to just accept it as factual about the code regardless. This is actually an improvement over also reporting an error on the second line.

```lua
local x: number = "hello %s" -- type error, number and string mismatches
local y = x:format("world") -- no type errors, `x` is actually a string!
```

### Local initialization with explicit `nil` are no longer make-pretend.

```lua
local x = nil
local y: string = x -- now a type error!
```

Similarly, type states can now properly track implicit `nil`.

```lua
local function f() return "hi" end

local x, y = f(), 5
x, y = f()

local z: string = y -- type error, y is nil
```

### Combination of upper and lower bound inference.

Something that we can do with type states is to _also_ infer singleton types without them getting in the way, even though `x` was not explicitly ascribed a singleton type.

```lua
local x = "hello!"
local y: "hello!" = x -- no type errors!
```

## Drawbacks

Something something it's not a perfect solution, something something alias analysis.

This isn't a be all end all solution. There are cases where we may end up introducing unsoundness. Here's a trivial example:

```lua
local t: {x: number | string} = {x = 5}
local u = t

t.x = "hi"

local a: string = t.x
local b: string = u.x
```

And the reason for that is because we only track the state by the syntactic structure of the lvalues themselves. This is a drawback that existed anyway with refinements, so no difference other than increasing the surface area of this particular limitation.

Also, because we are not implementing control flow analysis yet, there will be a subset of type errors which are false positives. Generally as a direct result of loopy constructs or some change in control flow that we aren't tracking.

For example, in the body after the `if`, we have no choice but to deduce the type `number | string` because we do not know that the block of the assignment `x = "hi"` just terminates the function, rendering the assignment useless and irrelevant.

```lua
local function f(x: number | string)
    if type(x) == "string" then
        x = "hi"
        return
    else
        x = 5
    end

    -- `x` should be of type `number`, but without control flow analysis, it will be of type `number | string`
end
```

## Alternatives

Do nothing and accept the false positives. This is obviously a non-starter.
