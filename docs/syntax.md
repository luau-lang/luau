Syntax
======

Luau uses the baseline [syntax of Lua 5.1](https://www.lua.org/manual/5.1/manual.html#2). For detailed documentation, please refer to the Lua manual, this is an example:

```lua
local function tree_insert(tree, x)
    local lower, equal, greater = split(tree.root, x)
    if not equal then
        equal = {
            x = x,
            y = math.random(0, 2^31-1),
            left = nil,
            right = nil
        }
    end
    tree.root = merge3(lower, equal, greater)
end
```

Note that future versions of Lua extend the Lua 5.1 syntax with the following features; with the exception of the string literals, these are **not** supported by Luau:

- hexadecimal (`\0x`), Unicode (`\u`) and `\z` escapes for string literals
- goto statements and labels
- bitwise operators
- floor division operator (`//`)
- `<toclose>` and `<const>` local attributes

> For details please refer to [compatibility section](compatibility.md).

The rest of this document documents additional syntax used in Luau.

String literals
===============

As noted above, Luau implements support for hexadecimal (`\0x`), Unicode (`\u`) and `\z` escapes for string literals. This syntax follows [Lua 5.3 syntax](https://www.lua.org/manual/5.3/manual.html#3.1):

- `\0xAB` inserts a character with the code 0xAB into the string
- `\u{ABC}` inserts a UTF8 byte sequence that encodes U+0ABC character into the string (note that braces are mandatory)
- `\z` at the end of the line inside a string literal ignores all following whitespace including newlines, which can be helpful for breaking long literals into multiple lines.

Number literals
===============

In addition to basic integer and floating-point decimal numbers, Luau supports:

- Hexadecimal integer literals, `0xABC` or `0XABC`
- Binary integer literals, `0b01010101` or `0B01010101`
- Decimal separators in all integer literals, using `_` for readability: `1_048_576`, `0xFFFF_FFFF`, `0b_0101_0101`

Note that Luau only has a single number type, a 64-bit IEEE754 double precision number (which can represent integers up to 2^53 exactly), and larger integer literals are stored with precision loss.

Continue statement
==================

In addition to `break` in all loops, Luau supports `continue` statement. Similar to `break`, `continue` must be the last statement in the block.

Note that unlike `break`, `continue` is not a keyword. This is required to preserve backwards compatibility with existing code; so this is a `continue` statement:

```lua
if x < 0 then
    continue
end
```

Whereas this is a function call:

```lua
if x < 0 then
    continue()
end
```

When used in `repeat..until` loops, `continue` can not skip the declaration of a local variable if that local variable is used in the loop condition; code like this is invalid and won't compile:

```lua
repeat
    do continue end
    local a = 5
until a > 0
```

Compound assignments
====================

Luau supports compound assignments with the following operators: `+=`, `-=`, `*=`, `/=`, `%=`, `^=`, `..=`. Just like regular assignments, compound assignments are statements, not expressions:

```lua
-- this works
a += 1

-- this doesn't work
print(a += 1)
```

Compound assignments only support a single value on the left and right hand side; additionally, the function calls on the left hand side are only evaluated once:

```lua
-- calls foo() twice
a[foo()] = a[foo()] + 1

-- calls foo() once
a[foo()] += 1
```

Compound assignments call the arithmetic metamethods (`__add` et al) and table indexing metamethods (`__index` and `__newindex`) as necessary - for custom types no extra effort is necessary to support them.

Type annotations
================

To support gradual typing, Luau supports optional type annotations for variables and functions, as well as declaring type aliases.

Types can be declared for local variables, function arguments and function return types using `:` as a separator:

```lua
function foo(x: number, y: string): boolean
    local k: string = y:rep(x)
    return k == "a"
end
```

There are several simple builtin types: `any` (represents inability of the type checker to reason about the type), `nil`, `boolean`, `number`, `string` and `thread`.

Function types are specified using the arguments and return types, separated with `->`:

```lua
local foo: (number, string) -> boolean
```

Table types are specified using the table literal syntax, using `:` to separate keys from values:

```lua
local array: { [number] : string }
local object: { x: number, y: string }
```

Additionally, the type syntax supports type intersections (`(number) -> string & (boolean) -> string`) and unions (`(number | boolean) -> string`). An intersection represents a type with values that conform to both sides at the same time, which is useful for overloaded functions; a union represents a type that can store values of either type - `any` is technically a union of all possible types.

It's common in Lua for function arguments or other values to store either a value of a given type or `nil`; this is represented as a union (`number | nil`), but can be specified using `?` as a shorthand syntax (`number?`).

In addition to declaring types for a given value, Luau supports declaring type aliases via `type` syntax:

```lua
type Point = { x: number, y: number }
type Array<T> = { [number]: T }
type Something = typeof(string.gmatch("", "\d"))
```

The right hand side of the type alias can be a type definition or a `typeof` expression; `typeof` expression doesn't evaluate its argument at runtime.

By default type aliases are local to the file they are declared in. To be able to use type aliases in other modules using `require`, they need to be exported:

```lua
export type Point = { x: number, y: number }
```

For more information please refer to [typechecking documentation](typecheck.md).
