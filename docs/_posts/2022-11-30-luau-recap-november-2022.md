---
layout: single
title:  "Luau Recap: November 2022"
---

While the team is busy to bring some bigger things in the future, we have made some small improvements this month.

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-november-2022/).]

## Error messages

Error about function argument count mismatch no longer points at the last argument, but instead at the function in question.
So, instead of:

```lua
function myfunction(a: number, b:number) end
myfunction(123)
           ~~~
```

We now highlight this:

```lua
function myfunction(a: number, b:number) end
myfunction(123)
~~~~~~~~~~
```

Parsing now recovers with a more precise error message if you forget a comma in table constructor spanning multiple lines:

```lua
local t = {
    a = 1
    b = 2 -- Expected ',' after table constructor element
    c = 3 -- Expected ',' after table constructor element
}
```

If you iterate over a table value that could also be `nil`, you get a better explanation in the error message:

```lua
local function f(t: {number}?)
    for i,v in t do -- Value of type {number}? could be nil
        --...
    end
end
```
Previously it was `Cannot call non-function {number}?` which was confusing.

And speaking of confusing, some of you might have seen an error like `Type 'string' could not be converted into 'string'`.

This was caused by Luau having both a primitive type `string` and a table type coming from `string` library. Since the way you can get the type of the `string` library table is by using `typeof(string)`, the updated error message will mirror that and report `Type 'string' could not be converted into 'typeof(string)'`.

## Analysis improvements

For better inference, we updated definition of `Enum.SomeType:GetEnumItems()` to return `{Enum.SomeType}` instead of common `{EnumItem}` and the return type of `next` function now includes the possibility of key being `nil`.

If you use `and` operator on non-boolean values, `boolean` type will no longer be added by the type inference:

```lua
local function f1(a: number?)
    -- 'x' is still a 'number?' and doesn't become 'boolean | number'
    local x = a and 5
end
```
