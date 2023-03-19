# Floor division operator

## Summary

Add floor division operator `//` to ease computing with integers.

## Motivation

Integers are everywhere. Indices, pixel coordinates, offsets, ranges, quantities, counters, rationals, fixed point arithmetic and bitwise operations all use integers.

Luau is generally well suited to work with integers. The math operators +, -, \*, ^ and % support integers. That is, given integer operands these operators produce an integer result (provided that the result fits into representable range of integers). However, that is not the case with the division operator `/` which in the general case produces numbers with fractionals.

To overcome this, typical Luau code performing integer computations needs to wrap the result of division inside a call to `math.floor`. This has a number of issues and can be error prone in practice.

A typical mistake is to forget to use `math.floor`. This can produce subtle issues ranging from slightly wrong results to script errors. A script error could occur, for example, when the result of division is used to fetch from a table with only integer keys, which produces nil and a script error happens soon after. Another type of error occurs when an accidental fractional number is passed to a C function. Depending on the implementation, the C function could raise an error (if it checks that the number is actually an integer) or cause logic errors due to rounding.

Particularly problematic is incorrect code which seems to work with frequently used data, only to fail with some rare input. For example, image sizes often have power of two dimensions, so code dealing with them may appear to work fine until much later some rare image has an odd size and a division by two in the code does not produce the correct result. Due to better ergonomics of the floor division operator, it becomes a second nature to write `//` everywhere when integers are involved and thus this class of bugs is much less likely to happen.

Another issue with using `math.floor` as a workaround is that code performing a lot of integer calculations is harder to understand, write and maintain.

Especially with applications dealing with pixel graphics, such as 2D games, integer math is so common that `math.floor` could easily become the most commonly used math library function. For these applications, avoiding the calls to `math.floor` is alluring from the performance perspective.

> Non-normative: Here are the top math library functions used by a shipped game that heavily uses Lua: 
> `floor`: 461 matches, `max`: 224 matches, `sin`: 197 matches, `min`: 195 matches, `clamp`: 171 matches, `cos`: 106 matches, `abs`: 85 matches.
> The majority of `math.floor` calls disappear from this codebase with the floor division operator.

Lua has had floor division operator since version 5.3, so its addition to Luau makes it easier to migrate from Lua to Luau and perhaps more importantly use the wide variety of existing Lua libraries in Luau. Of other languages, most notably Python has floor division operator with same semantics and same syntax. R and Julia also have a similar operator.

## Design

The design mirrors Lua 5.3:

New operators `//` and `//=` will be added to the language. The operator `//` performs division of two operands and rounds the result towards negative infinity. By default, the operator is only defined for numbers. The operator has the same precedence as the normal division operator `/`. `//=` is the compound-assignment operator for floor division, similar to the existing operator `/=`.

A new metamethod `__idiv` will be added. The metamethod is invoked when any operand of floor division is not a number. The metamethod can be used to implement floor division for any user defined data type as well as the built-in vector type.

The typechecker does not need special handling for the new operators. It can simply apply the same rules for floor division as it does for normal division operators.

Examples of usage:

```
-- Convert offset into 2d indices
local i, j = offset % 5, offset // 5

-- Halve dimensions of an image or UI element
width, height = width // 2, height // 2

-- Draw an image to the center of the window
draw_image(image, window_width // 2 - element_width // 2, window_height // 2 - element_height // 2)
```

## Drawbacks

The addition of the new operator adds some complexity to the implementation (mostly to the VM) and to the language, which can be seen as a drawback.

C like languages use `//` for line comments. Using the symbol `//` for floor division closes the door for using it for line comments in Luau. On the other hand, Luau already has long and short comment syntax, so adding yet another syntax for comments would add complexity to the language for little benefit. Moreover, it would make it harder to translate code from Lua to Luau and use existing Lua libraries if the symbol `//` has a completely different meaning in Lua and Luau.

## Alternatives

An alternative would be to do nothing but this would not solve the issues the lack of floor division currently has.

An alternative implementation would treat `//` and `//=` only as syntactic sugar. The addition of new VM opcode for floor division could be omitted and the compiler could be simply modified to automatically emit a call to `math.floor` when necessary. This would require only minimal changes to Luau, but it would not support overloading the floor division operator using metamethods and would not have the performance benefits of the full implementation.
