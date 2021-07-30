---
layout: single
title:  "Luau Recap: July 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org). Our team was still busy working on upcoming Studio Beta feature for script editor, but we did fit in multiple typechecking improvements.

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-july-2021/).]

## Typechecking improvements

A common complaint that we've received was a false-positive error when table with an optional or union element type is defined:
```lua
--!strict
type Foo = {x: number | string}
local foos: {Foo} = {
    {x = 1234567},
    {x = "hello"} -- Type 'string' could not be converted into 'number'
}
```
This case is now handled and skipping optional fields is allowed as well:
```lua
--!strict
type Foo = {
    a: number,
    b: number?
}
local foos: {Foo} = {
    { a = 1 },
    { a = 2, b = 3 } -- now ok
}
```
Current fix only handles table element type in assignments, but we plan to extend that to function call arguments and individual table fields.

Like we've mentioned last time, we will continue working on our new type constraint resolver and this month it learned to handle more complex expressions (including type guards) inside `assert` conditions:
```lua
--!strict
local part = script.Parent:WaitForChild("Part")
assert(part:IsA("BasePart"))
local basepart: BasePart = part -- no longer an error
```

And speaking of assertions, we applied a minor fix so that the type of the `assert` function correctly defines a second optional `string?` parameter.

We have also fixed the type of `string.gmatch` function reported by one of the community members.
We know about issues in a few additional library functions and we'll work to fix them as well.

Hopefully, you didn't see 'free type leak' errors that underline your whole script, but some of you did and reported them to us.
We read those reports and two additional cases have been fixed this month.
We now track only a single one that should be fixed next month.

Another false positive error that was fixed involves tables with __call metatable function.
We no longer report a type error when this method is invoked and we'll also make sure that given arguments match the function definition:
```lua
--!strict
local t = { x = 2 }

local x = setmetatable(t, {
    __call = function(self, a: number)
        return a * self.x
    end
})
local a = x(2) -- no longer an error
```
Please note that while call operator on a table is now handled, function types in Luau are distinct from table types and you'll still get an error if you try to assign this table to a variable of a function type.

## Linter improvements

A new 'TableOperations' lint check was added that will detect common correctness or performance issues with `table.insert` and `table.remove`:
```lua
-- table.insert will insert the value before the last element, which is likely a bug; consider removing the second argument or wrap it in parentheses to silence
table.insert(t, #t, 42)

-- table.insert will append the value to the table; consider removing the second argument for efficiency
table.insert(t, #t + 1, 42)

-- table.insert uses index 0 but arrays are 1-based; did you mean 1 instead?
table.insert(t, 0, 42)

-- table.remove uses index 0 but arrays are 1-based; did you mean 1 instead?
table.remove(t, 0)

-- table.remove will remove the value before the last element, which is likely a bug; consider removing the second argument or wrap it in parentheses to silence
table.remove(t, #t - 1)

-- table.insert may change behavior if the call returns more than one result; consider adding parentheses around second argument
table.insert(t, string.find("hello", "h"))
```

Another new check is 'DuplicateConditions'. The name speaks for itself, `if` statement chains with duplicate conditions and expressions containing `and`/`or` operations with redundant parts will now be detected:
```lua
if x then
    -- ...
elseif not x then
    -- ...
elseif x̳ then -- Condition has already been checked on line 1
    -- ...
end

local success = a and a̳ -- Condition has already been checked on column 17

local good = (a or b) or a̳ -- Condition has already been checked on column 15
``` 

We've also fixed an incorrect lint warning when `typeof` is used to check for `EnumItem`.

## Editor features

An issue was fixed that prevented the debugger from displaying values inside Roblox callback functions when an error was reported inside of it.

## Behavior changes

`table.insert` will no longer move elements forward 1 spot when index is negative or 0.

This change also fixed a performance issue when `table.insert` was called with a large negative index.

The 'TableOperations' lint mentioned earlier will flag cases where insertion at index 0 is performed.
