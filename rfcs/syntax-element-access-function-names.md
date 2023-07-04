# Element Access Function Names

# Summary

Add syntax for creating functions with element access function names similarly to member and method function names. The ability to chain [<expr>] is allowed as well.

`function <ident>[<expr>]()`

```lua
local t = {}
local example_data = newproxy()
function t[example_data](output)
    print(output)
end

t[example_data]("Hello world, from Luau!")
```

# Motivation

Adding this would be consistent with other function name syntax and produces more readable code in most cases. Future motivations of this syntax could enable easier optimizations with regards to table accesses.

As an example, on Roblox these changes enable working with instances in tables in a more readable way. A look-up table of functions linked to players will be able to be written as `function PlayerFunctions[Player]()` and can be accessed with `PlayerFunctions[Player]()`.

The addition of this syntax also makes calling syntax parallel, where `function table.member()` is called with `table.member()` and `function table[element]()` is called with `table[element]()`.

# Drawbacks

The addition of new syntactic sugar adds more complexity to the compiler and to the language, which can be considered a drawback. 

# Alternatives

The alternative would be to do nothing.
