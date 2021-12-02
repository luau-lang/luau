---
layout: single
title:  "Luau Recap: November 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-november-2021/).]

## Singleton types

Singleton types, also known as literal types, allow you to represent a specific value of a string or boolean in the type system. This is useful for representing enums and similar constructs.

```lua
type Status = "ok" | "error"

local x: Status = "ok"
```

At the moment, support for these is limited to just the type construct. In the near future, Luau will make use of this information to better infer `if` statements and expressions:

```lua
type Result = {
    success: true,
    value: number,
}

type Error = {
    success: false,
    error: string,
}

type ResultOrError = Result | Error
local x: ResultOrError

if x.success then
    -- x is inferred to be of type Result
    print(x.value * 2)
else
    -- x is inferred to be of type Error
    print("error:", x.error)
end
```

## Type packs in type aliases

Type packs are the construct Luau uses to represent a sequence of types. We've had syntax for generic type packs for a while now, and it sees use in generic functions, but it hasn't been available in type aliases. That has changed, and it is now syntactically legal to write the following type alias:
```lua
type X<A...> = () -> A...
type Y = X<number, string>
```

We've also added support for explicit type packs. Previously, it was impossible to instantiate a generic with two or more type pack parameters, because it wasn't clear where the first pack ended and the second one began. We have introduced a new syntax for this use case:
```
type Fn<P..., R...> = (P...) -> R...
type X = Fn<(number, string), (string, number)>
```

For more information, check out [the RFC for this feature](https://github.com/Roblox/luau/blob/f86d4c6995418e489a55be0100159009492778ff/rfcs/syntax-type-alias-type-packs.md).

## Library improvements

```lua
function bit32.countlz(n: number): number
function bit32.countrz(n: number): number
```
Given a number, returns the number of preceding left or trailing right-hand bits that are `0`.

See [the RFC for these functions](https://github.com/Roblox/luau/blob/f86d4c6995418e489a55be0100159009492778ff/rfcs/function-bit32-countlz-countrz.md) for more information.
