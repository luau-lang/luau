# String interpolation

## Summary

New string interpolation syntax, and extending `string.format` to accept any arbitrary values without being exact about its types.

## Motivation

The problems with `string.format` are many.

1. Must be exact about the types and its corresponding value.
2. Using `%d` is the idiomatic default for most people, but this loses precision.
   * `%d` casts the number into `long long`, which has a lower max value than `double` and does not support decimals.
   * `%f` by default will format to the millionths, e.g. `5.5` is `5.500000`.
   * `%g` by default will format up to the hundred thousandths, e.g. `5.5` is `5.5` and `5.5312389` is `5.53123`. It will also convert the number to scientific notation when it encounters a number equal to or greater than 10^6.
   * To not lose precision, you need to use `%s`, but even so the type checker assumes you actually wanted strings.
3. No support for `boolean`. You must use `%s` **and** call `tostring`.
4. No support for values implementing the `__tostring` metamethod.
5. Having to use parentheses around string literals just to call a method of it.

## Design

To fix issue #5, we will need new syntax in order to not change the meaning of existing strings. There are a few components of this new expression:

1. A string chunk (`` `...{ ``, `}...{`, and `` }...` ``) where `...` is a range of 0 to many characters.
   * `%` escapes `%` and `{`, whereas `\` escapes `` ` ``.
   * Restriction: the string interpolation literal must have at least one value to interpolate. We do not need 3 ways to express a single line string literal.
2. An expression between the braces. This is the value that will be interpolated into the string.
3. Formatting style may follow the expression, delimited by `,`.
   * Restriction: the formatting style must be constant at parse time.
   * The formatting style will follow the same syntax as pre-existing syntax in `string.format`.

To put the above into formal EBNF grammar:

```
fmtflags ::= `-' | `+' | ` ' | `#' | `0'
fmtwidth ::= <NUMBER> [<NUMBER>]
fmtprecision ::= `.' <NUMBER> [<NUMBER>]
fmtspecifier ::= `c' | `d' | `i' | `e' | `E' | `f' | `g' | `G' | `o' | `q' | `s' | `u' | `x' | `X'

interpfmt ::= [fmtflags][fmtwidth][fmtprecision][fmtspecifier]

interpexp ::= exp [`,' interpfmt]

stringinterp ::= <INTERP_BEGIN> interpfmt {<INTERP_MID> interpexp} <INTERP_END>
```

Which, in actual Luau code, will look like the following:

```
local world = "world"
print(`Hello {world}!`)
--> Hello world!

print(`0x{3735928559,X}`)
--> 0xDEADBEEF

local combo = {5, 2, 8, 9}
print(`The lock combinations are: {table.concat(combo, ", ")}`)
--> The lock combinations are: 5, 2, 8, 9
```

To fix issues #1, #2, #3, and #4, we will also want to extend `string.format` to accept braces after `%`. This is currently an invalid token, so this is a backward compatible extension. Unfortunately, formatting style cannot immediately follow `%` because `%d{}` is already valid and parses as `(%d){}`.

The extended syntax will allow the following ``%{[<NUMBER>][`,' interpfmt]}`` to be parsed as one token.

To put that into perspective:

```lua
print(string.format("%{} %{}", 1, 2))
--> 1 2

print(string.format("%{2} %{1}", 1, 2))
--> 2 1

print(string.format("0x%{,X}", 3735928559))
--> 0xDEADBEEF
```

One additional thing to be aware of is that explicit positional numbers will not affect `string.format`'s internal offset. That is, `%{2}` will immediately get the 2nd value, irrespective of the offset. `%{}` will increment the offset and then get the value at that offset.

```lua
print(string.format("%{2} %{} %{} %{1} %{}", 1, 2, 3))
--> 2 1 2 1 3
```

The offset must always be within bound of the numbers of values passed to `string.format`.

```lua
local function return_nothing() end
local function return_two_nils() return nil, nil end

print(string.format("%{2} %{}", return_nothing()))
--> error: missing arguments

print(string.format("%{2} %{}", return_two_nils()))
--> nil nil
```

## Drawbacks

If we want to use backticks for other purposes, it may introduce some potential ambiguity. One option to solve that is to only ever produce string interpolation tokens from the context of an expression. This is messy but doable because the parser and the lexer are already implemented to work in tandem. The other option is to pick a different delimiter syntax to keep backticks available for use in the future.

If we were to naively compile the expression into a `string.format` call, then implementation details would be observable if you write `` `Your health is {hp}% so you need to heal up.` ``. We would need to implicitly insert a `%` character anytime one shows up in a string interpolation token (escaping `%` wouldn't show up in the token at all). Otherwise attempting to run this will produce a runtime error where the `%s` token is missing its corresponding string value. 

This also increases the complexity of parsing `string.format` across the board. We would now have 3 copies of the parsing logic, one in the parser, one in the VM, and one in the type checker. Having a single source of truth for the syntax of `string.format` is ideal, but may be futile.

Another drawback is that we might lose an opportunity to come up with a new formatting style syntax and semantics. If we care about that opportunity, we could leave that to be undefined in this RFC and have a future RFC define it. For example, `string.format` is missing a way to center-justify a value with a fill character. Hexadecimal/octal conversions does not include the prefix. There's also no binary conversion. If we take `string.format`'s existing formatting style syntax, it may not be compatible with future formatting style extensions.

## Alternatives

Language   | Syntax                | Conclusion
----------:|:----------------------|:-----------
Python     | `f'Hello {name}'`     | Rejected because it's ambiguous with function call syntax.
Swift      | `"Hello \(name)"`     | Rejected because it changes the meaning of existing strings.
Ruby       | `"Hello #{name}"`     | Rejected because it changes the meaning of existing strings.
JavaScript | `` `Hello ${name}` `` | Viable option as long as we don't intend to use backticks for other purposes.
C#         | `$"Hello {name}"`     | Viable option and guarantees no ambiguities with future syntax.

This leaves us with only two syntax that already exists in other programming languages. The current proposal are for backticks, so the only backward compatible alternative are `$""` literals. We don't necessarily need to use `$` symbol here, but if we were to choose a different symbol, `#` cannot be used.

I picked backticks because it doesn't require us to add a stack of closing delimiters in the lexer to make sure each nested string interpolation literals are correctly closed with its opening pair. You only have to count them.
