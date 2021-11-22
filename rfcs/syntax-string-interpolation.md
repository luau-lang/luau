# String interpolation

## Summary

New string interpolation syntax.

## Motivation

The problems with `string.format` are many.

1. Must be exact about the types and its corresponding value.
2. Using `%d` is the idiomatic default for most people, but this loses precision.
   * `%d` casts the number into `long long`, which has a lower max value than `double` and does not support decimals.
   * `%f` by default will format to the millionths, e.g. `5.5` is `5.500000`.
   * `%g` by default will format up to the hundred thousandths, e.g. `5.5` is `5.5` and `5.5312389` is `5.53123`. It will also convert the number to scientific notation when it encounters a number equal to or greater than 10^6.
   * To not lose too much precision, you need to use `%s`, but even so the type checker assumes you actually wanted strings.
3. No support for `boolean`. You must use `%s` **and** call `tostring`.
4. No support for values implementing the `__tostring` metamethod.
5. Using `%` is in itself a dangerous operation within `string.format`.
   * `"Your health is %d% so you need to heal up."` causes a runtime error because `% so` is actually parsed as `(%s)o` and now requires a corresponding string.
6. Having to use parentheses around string literals just to call a method of it.

## Design

To fix all of those issues, we need to do a few things.

1. A new string interpolation expression (fixes #5, #6)
2. Extend `string.format` to accept values of arbitrary types (fixes #1, #2, #3, #4)

Because we care about backward compatibility, we need some new syntax in order to not change the meaning of existing strings. There are a few components of this new expression:

1. A string chunk (`` `...{ ``, `}...{`, and `` }...` ``) where `...` is a range of 0 to many characters.
   * `\` escapes `` ` ``, `{`, and itself `\`.
   * Restriction: the string interpolation literal must have at least one value to interpolate. We do not need 3 ways to express a single line string literal.
   * The pairs must be on the same line (unless a `\` escapes the newline) but expressions needn't be on the same line.
2. An expression between the braces. This is the value that will be interpolated into the string.
3. Formatting specification may follow after the expression, delimited by an unambiguous character.
   * Restriction: the formatting specification must be constant at parse time.
   * In the absence of an explicit formatting specification, the `%*` token will be used.
   * For now, we explicitly reject any formatting specification syntax. A future extension may be introduced to extend the syntax with an optional specification.

To put the above into formal EBNF grammar:

```
stringinterp ::= <INTERP_BEGIN> exp {<INTERP_MID> exp} <INTERP_END>
```

Which, in actual Luau code, will look like the following:

```
local world = "world"
print(`Hello {world}!`)
--> Hello world!

local combo = {5, 2, 8, 9}
print(`The lock combinations are: {table.concat(combo, ", ")}`)
--> The lock combinations are: 5, 2, 8, 9

local set1 = Set.new({0, 1, 3})
local set2 = Set.new({0, 5, 4})
print(`{set1} ∪ {set2} = {Set.union(set1, set2)}`)
--> {0, 1, 3} ∪ {0, 5, 4} = {0, 1, 3, 4, 5}

-- For illustrative purposes. These are illegal specifically because they don't interpolate anything.
print(`Some example escaping the braces \{like so}`)
print(`backslash \ that escapes the space is not a part of the string...`)
print(`backslash \\ will escape the second backslash...`)
print(`Some text that also includes \`...`)
--> Some example escaping the braces {like so}
--> backslash  that escapes the space is not a part of the string...
--> backslash \ will escape the second backslash...
--> Some text that also includes `...
```

As for how newlines are handled, they are handled the same as other string literals. Any text between the `{}` delimiters are not considered part of the string, hence newlines are OK. The main thing is that one opening pair will scan until either a closing pair is encountered, or an unescaped newline.

```
local name = "Luau"

print(`Welcome to {
    name
}!`)
--> Welcome to Luau!

print(`Welcome to \
{name}!`)
--> Welcome to
--  Luau!
```

This expression will not be allowed to come after a `prefixexp`. I believe this is fully additive, so a future RFC may allow this. So for now, we explicitly reject the following:

```
local name = "world"
print`Hello {name}`
```

Since the string interpolation expression is going to be lowered into a `string.format` call, we'll also need to extend `string.format`. The bare minimum to support the lowering is to add a new token whose definition is to perform a `tostring` call. `%*` is currently an invalid token, so this is a backward compatible extension. This RFC shall define `%*` to have the same behavior as if `tostring` was called.

```lua
print(string.format("%* %*", 1, 2))
--> 1 2
```

The offset must always be within bound of the numbers of values passed to `string.format`.

```lua
local function return_one_thing() return "hi" end
local function return_two_nils() return nil, nil end

print(string.format("%*", return_one_thing()))
--> "hi"

print(string.format("%*", Set.new({1, 2, 3})))
--> {1, 2, 3}

print(string.format("%* %*", return_two_nils()))
--> nil nil

print(string.format("%* %* %*", return_two_nils()))
--> error: value #3 is missing, got 2
```

## Drawbacks

If we want to use backticks for other purposes, it may introduce some potential ambiguity. One option to solve that is to only ever produce string interpolation tokens from the context of an expression. This is messy but doable because the parser and the lexer are already implemented to work in tandem. The other option is to pick a different delimiter syntax to keep backticks available for use in the future.

If we were to naively compile the expression into a `string.format` call, then implementation details would be observable if you write `` `Your health is {hp}% so you need to heal up.` ``. When lowering the expression, we would need to implicitly insert a `%` character anytime one shows up in a string interpolation token. Otherwise attempting to run this will produce a runtime error where the `%s` token is missing its corresponding string value.

## Alternatives

Rather than coming up with a new syntax (which doesn't help issue #5 and #6) and extending `string.format` to accept an extra token, we could just make `%s` call `tostring` and be done. However, doing so would cause programs to be more lenient and the type checker would have no way to infer strings from a `string.format` call. To preserve that, we would need a different token anyway.

Language   | Syntax                | Conclusion
----------:|:----------------------|:-----------
Python     | `f'Hello {name}'`     | Rejected because it's ambiguous with function call syntax.
Swift      | `"Hello \(name)"`     | Rejected because it changes the meaning of existing strings.
Ruby       | `"Hello #{name}"`     | Rejected because it changes the meaning of existing strings.
JavaScript | `` `Hello ${name}` `` | Viable option as long as we don't intend to use backticks for other purposes.
C#         | `$"Hello {name}"`     | Viable option and guarantees no ambiguities with future syntax.

This leaves us with only two syntax that already exists in other programming languages. The current proposal are for backticks, so the only backward compatible alternative are `$""` literals. We don't necessarily need to use `$` symbol here, but if we were to choose a different symbol, `#` cannot be used. I picked backticks because it doesn't require us to add a stack of closing delimiters in the lexer to make sure each nested string interpolation literals are correctly closed with its opening pair. You only have to count them.
