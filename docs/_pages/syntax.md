---
permalink: /syntax
title: Syntax
toc: true
---

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

Note that future versions of Lua extend the Lua 5.1 syntax with more  features; Luau does  support string literal extensions but does not support other 5.x additions; for details please refer to [compatibility section](compatibility).

The rest of this document documents additional syntax used in Luau.

## String literals

Luau implements support for hexadecimal (`\x`), Unicode (`\u`) and `\z` escapes for string literals. This syntax follows [Lua 5.3 syntax](https://www.lua.org/manual/5.3/manual.html#3.1):

- `\xAB` inserts a character with the code 0xAB into the string
- `\u{ABC}` inserts a UTF8 byte sequence that encodes U+0ABC character into the string (note that braces are mandatory)
- `\z` at the end of the line inside a string literal ignores all following whitespace including newlines, which can be helpful for breaking long literals into multiple lines.

## Number literals

In addition to basic integer and floating-point decimal numbers, Luau supports:

- Hexadecimal integer literals, `0xABC` or `0XABC`
- Binary integer literals, `0b01010101` or `0B01010101`
- Decimal separators in all integer literals, using `_` for readability: `1_048_576`, `0xFFFF_FFFF`, `0b_0101_0101`

Note that Luau only has a single number type, a 64-bit IEEE754 double precision number (which can represent integers up to 2^53 exactly), and larger integer literals are stored with precision loss.

## Continue statement

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

## Compound assignments

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

Compound assignments call the arithmetic metamethods (`__add` et al) and table indexing metamethods (`__index` and `__newindex`) as needed - for custom types no extra effort is necessary to support them.

## Type annotations

To support gradual typing, Luau supports optional type annotations for variables and functions, as well as declaring type aliases.

Types can be declared for local variables, function arguments and function return types using `:` as a separator:

```lua
function foo(x: number, y: string): boolean
    local k: string = y:rep(x)
    return k == "a"
end
```

In addition, the type of any expression can be overridden using a type cast `::`:

```lua
local k = (y :: string):rep(x)
```

There are several simple builtin types: `any` (represents inability of the type checker to reason about the type), `nil`, `boolean`, `number`, `string` and `thread`.

Function types are specified using the arguments and return types, separated with `->`:

```lua
local foo: (number, string) -> boolean
```

To return no values or more than one, you need to wrap the return type position with parentheses, and then list your types there.

```lua
local no_returns: (number, string) -> ()
local returns_boolean_and_string: (number, string) -> (boolean, string)

function foo(x: number, y: number): (number, string)
    return x + y, tostring(x) .. tostring(y)
end
```

Note that function types are specified without the argument names in the examples above, but it's also possible to specify the names (that are not semantically significant but can show up in documentation and autocomplete):

```lua
local callback: (errorCode: number, errorText: string) -> ()
```

Table types are specified using the table literal syntax, using `:` to separate keys from values:

```lua
local array: { [number] : string }
local object: { x: number, y: string }
```

When the table consists of values keyed by numbers, it's called an array-like table and has a special short-hand syntax, `{T}` (e.g. `{string}`).

Additionally, the type syntax supports type intersections (`((number) -> string) & ((boolean) -> string)`) and unions (`(number | boolean) -> string`). An intersection represents a type with values that conform to both sides at the same time, which is useful for overloaded functions; a union represents a type that can store values of either type - `any` is technically a union of all possible types.

It's common in Lua for function arguments or other values to store either a value of a given type or `nil`; this is represented as a union (`number | nil`), but can be specified using `?` as a shorthand syntax (`number?`).

In addition to declaring types for a given value, Luau supports declaring type aliases via `type` syntax:

```lua
type Point = { x: number, y: number }
type Array<T> = { [number]: T }
type Something = typeof(string.gmatch("", "%d"))
```

The right hand side of the type alias can be a type definition or a `typeof` expression; `typeof` expression doesn't evaluate its argument at runtime.

By default type aliases are local to the file they are declared in. To be able to use type aliases in other modules using `require`, they need to be exported:

```lua
export type Point = { x: number, y: number }
```

An exported type can be used in another module by prefixing its name with the require alias that you used to import the module.

```lua
local M = require(Other.Module)

local a: M.Point = {x=5, y=6}
```

For more information please refer to [typechecking documentation](typecheck).

## If-then-else expressions

In addition to supporting standard if *statements*, Luau adds support for if *expressions*.  Syntactically, `if-then-else` expressions look very similar to if statements.  However instead of conditionally executing blocks of code, if expressions conditionally evaluate expressions and return the value produced as a result. Also, unlike if statements, if expressions do not terminate with the `end` keyword.

Here is a simple example of an `if-then-else` expression:
```lua
local maxValue = if a > b then a else b
```

`if-then-else` expressions may occur in any place a regular expression is used.  The `if-then-else` expression must match `if <expr> then <expr> else <expr>`; it can also contain an arbitrary number of `elseif` clauses, like `if <expr> then <expr> elseif <expr> then <expr> else <expr>`. Note that in either case, `else` is mandatory.  

Here's is an example demonstrating `elseif`:
```lua
local sign = if x < 0 then -1 elseif x > 0 then 1 else 0
```

**Note:** In Luau, the `if-then-else` expression is preferred vs the standard Lua idiom of writing `a and b or c` (which roughly simulates a ternary operator).  However, the Lua idiom may return an unexpected result if `b` evaluates to false.  The `if-then-else` expression will behave as expected in all situations.

## Generalized iteration

Luau uses the standard Lua syntax for iterating through containers, `for vars in values`, but extends the semantics with support for generalized iteration. In Lua, to iterate over a table you need to use an iterator like `next` or a function that returns one like `pairs` or `ipairs`. In Luau, you can simply iterate over a table:

```lua
for k, v in {1, 4, 9} do
    assert(k * k == v)
end
```

This works for tables but can also be extended for tables or userdata by implementing `__iter` metamethod that is called before the iteration begins, and should return an iterator function like `next` (or a custom one):

```lua
local obj = { items = {1, 4, 9} }
setmetatable(obj, { __iter = function(o) return next, o.items end })

for k, v in obj do
    assert(k * k == v)
end
```

The default iteration order for tables is specified to be consecutive for elements `1..#t` and unordered after that, visiting every element; similarly to iteration using `pairs`, modifying the table entries for keys other than the current one results in unspecified behavior.

## String interpolation

Luau adds an additional way to define string values that allows you to place runtime expressions directly inside specific spots of the literal.

This is a more ergonomic alternative over using `string.format` or `("literal"):format`.

To use string interpolation, use a backtick string literal:

```lua
local count = 3
print(`Bob has {count} apple(s)!`)
--> Bob has 3 apple(s)!
```

Any expression can be used inside `{}`:

```lua
local combos = {2, 7, 1, 8, 5}
print(`The lock combination is {table.concat(combos)}.`)
--> The lock combination is 27185.
```

Inside backtick string literal, `\` is used to escape `` ` ``, `{`, `\` itself and a newline:

```lua
print(`Some example escaping the braces \{like so}`)
--> Some example escaping the braces {like so}

print(`Backslash \ that escapes the space is not a part of the string...`)
--> Backslash  that escapes the space is not a part of the string...

print(`Backslash \\ will escape the second backslash...`)
--> Backslash \ will escape the second backslash...

print(`Some text that also includes \`...`)
--> Some text that also includes `...

local name = "Luau"

print(`Welcome to {
    name
}!`)
--> Welcome to Luau!
```

### Restrictions and limitations

The sequence of two opening braces {{"`{{`"}} is rejected with a parse error.
This restriction is made to prevent developers using other programming languages with a similar feature from trying to attempt that as a way to escape a single `{` and getting unexpected results in Luau.

Luau currently does not support backtick string literals as a type annotation, so `` type Foo = `Foo` `` is invalid.

Function calls with a backtick string literal without parenthesis is not supported, so `` print`hello` `` is invalid.
