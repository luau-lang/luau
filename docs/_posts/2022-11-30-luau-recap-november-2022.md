---
layout: single
title:  "Luau Recap: November 2022"
---

While the team is busy to bring some bigger things in the future, we have made some small improvements this month.

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-november-2022/).]

## Analysis improvements

We have improved tagged union type refinements to only include unhandled type cases in the `else` branch of the `if` statement:

```lua
type Ok<T> = { tag: "ok", value: T }
type Err = { tag: "error", msg: string }
type Result<T> = Ok<T> | Err

function unwrap<T>(r: Result<T>): T?
    if r.tag == "ok" then
        return r.value
    else
        -- Luau now understands that 'r' here can only be the 'Err' part
        print(r.msg)
        return nil
    end
end
```

For better inference, we updated the definition of `Enum.SomeType:GetEnumItems()` to return `{Enum.SomeType}` instead of common `{EnumItem}` and the return type of `next` function now includes the possibility of key being `nil`.

Finally, if you use `and` operator on non-boolean values, `boolean` type will no longer be added by the type inference:

```lua
local function f1(a: number?)
    -- 'x' is still a 'number?' and doesn't become 'boolean | number'
    local x = a and 5
end
```

## Error message improvements

We now give an error when built-in types are being redefined:

```lua
type string = number -- Now an error: Redefinition of type 'string'
```

We also had a parse error missing in case you forgot your default type pack parameter value. We accepted the following code silently without raising an issue:

```lua
type Foo<T... = > = nil -- Now an error: Expected type, got '>'
```

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


Parsing now recovers with a more precise error message if you forget a comma in table constructor spanning multiple lines:

```lua
local t = {
    a = 1
    b = 2 -- Expected ',' after table constructor element
    c = 3 -- Expected ',' after table constructor element
}
```
