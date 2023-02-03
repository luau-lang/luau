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

String interpolation also composes well with `__tostring` metamethod.

```lua
local balance = setmetatable({ value = 500 }, {
    __tostring = function(self)
        return "$" .. tostring(self.value)
    end
})

print(`You have {balance}!`)
--> You have $500!
```

Known Limitations
Script editor will not support auto-formatting string interpolations.

Luau currently does not define any format specifiers for string interpolations.

Luau currently does not support any_function_call`with backticks without parentheses`.

Luau currently does not support backtick string literals as a type annotation, so type Foo = `Foo` is invalid.

Please post any comments or concerns you may have!
