# Support bitwise operators

## Summary

Add bitwise operators and metamethods for bitwise operators

## Motivation

Luau currently implements the `bit32` library which adds support for 32 bit numbers. Supporting bitwise operators would add support for all numbers providing a plethora of benefits.

`bit32.x()` syntax can be complex to write and confuse users with many bit operations. Implementing bitwise operators would also improve performance compared to calling `bit32` functions and metamethod overhead would be less than doing `bit32.x()`, 

Implementing bitwise operators simplifies the learning complexity of learning the bitwise library due to the syntax being universal to most languages. 

## Design

```lua
x & y __band
x | y __bor
x ^^ y or x ~ y __bxor
~x __bnot
x << y __blshift
x >> y __brshift
```

Bxor support is odd because Lua 5.3 added the operation as `x ~ y` and a [Lua patch by Thierry Grellier and Joshua Simmons](https://lua-users.org/wiki/LuaPowerPatches) added it as `x ^^ y` 

## Drawbacks

Adding bitwise operators would add several new operations to the interpreter and would require new metamethods to be added. Increased compiler complexity.

## Alternatives

Implementing a `bit` library that would support all numbers
