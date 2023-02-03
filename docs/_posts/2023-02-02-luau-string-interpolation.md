---
layout: single
title: "String Interpolation"
---

String interpolation is the new syntax introduced to Luau that allows you to create a string literal with expressions inside of that string literal.

In short, it's a safer and more ergonomic alternative over `string.format`.

Here's a quick example of a string interpolation:

```lua
local combos = {2, 7, 1, 8, 5}
print(`The lock combination is {table.concat(combos)}. Again, {table.concat(combos, ", ")}.`)
--> The lock combination is 27185. Again, 2, 7, 1, 8, 5.
```

String interpolation also composes well with the `__tostring` metamethod.

```lua
local balance = setmetatable({ value = 500 }, {
    __tostring = function(self)
        return "$" .. tostring(self.value)
    end
})

print(`You have {balance}!`)
--> You have $500!
```

To find out more details about this feature, check out [Luau Syntax page](/syntax#string-interpolation).

This is also the first major language feature implemented in a [contribution](https://github.com/Roblox/luau/pull/614) from the open-source community. Thanks [Kampfkarren](https://github.com/Kampfkarren)!
