---
layout: single
title:  "Luau Recap: June 2021"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau). Most of our team was busy working on improving Luau interaction with Roblox Studio for an upcoming feature this month, but we were able to add typechecking and performance improvements as well!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-june-2021/).]

## Constraint Resolver

To improve type inference under conditional expressions and other dynamic type changes (like assignments) we have introduced a new constraint resolver framework into Luau type checker.

This framework allows us to handle more complex expressions that combine `and`/`not` operators and type guards.

Type guards support include expressions like:

* `if instance:IsA("ClassName") then`
* `if enum:IsA("EnumName") then`
* `if type(v) == "string" then`

This framework is extensible and we have plans for future improvements with `a == b`/`a ~= b` equality constraints and handling of table field assignments.

It is now also possible to get better type information inside `else` blocks of an `if` statement.

A few examples to see the constraint resolver in action:
```lua
function say_hello(name: string?)
    -- extra parentheses were enough to trip the old typechecker
    if (name) then 
        print("Hello " .. name .. "!")
    else
        print("Hello mysterious stranger!")
    end
end
```
```lua
function say_hello(name: string?, surname: string?)
    -- but now we handle that and more complex expressions as well
    if not (name and surname) then
        print("Hello mysterious stranger!")
    else
        print("Hello " .. name .. " " .. surname .. "!")
    end
end
```

Please note that constraints are currently placed only on local and global variables.
One of our goals is to include support for table members in the future.

## Typechecking improvements

We have improved the way we handled module `require` calls. Previously, we had a simple pattern match on the `local m = require(...)` statement, but now we have replaced it with a general handling of the function call in any context.

Handling of union types in equality operators was fixed to remove incorrect error reports.

A new `IsA` method was introduced to EnumItem to check the type of a Roblox Enum.
This is intended to replace the `enumItem.EnumType == Enum.NormalId` pattern in the code for a construct that allows our constraint resolver to infer better types.

Additional fixes include:
* `table.pack` return type was fixed
* A limit was added for deeply nested code blocks to avoid a crash
* We have improved the type names that are presented in error messages and Roblox Studio
* Error recovery was added to field access of a `table?` type. While you add a check for `nil`, typechecking can continue with better type information in other expressions.
* We handled a few internal compiler errors and rare crashes

## Editor features

If you have Luau-Powered Type Hover beta feature enabled in Roblox Studio, you will see more function argument names inside function type hovers.

## Behavior changes

We no longer allow referencing a function by name inside argument list of that function:

`local function f(a: number, b: typeof(f)) -- 'f' is no longer visible here`

## Performance improvements

As always, we look for ways to improve performance of your scripts:
* We have fixed memory use of Roblox Actor scripts in Parallel Luau beta feature
* Performance of table clone through `table.move` has been greatly improved
* Table length lookup has been optimized, which also brings improvement to table element insertion speed
* Built-in Vector3 type support that we mentioned in [April](https://devforum.roblox.com/t/native-luau-vector3-beta/) is now enabled for everyone
