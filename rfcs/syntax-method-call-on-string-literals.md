# Allow method call on string literals

> Note: this RFC was adapted from an internal proposal that predates RFC process

## Summary

Allow string literals to be indexed on without parentheses or from an identifier. That is, the following snippet will become legal under this proposal:

```lua
print("Hello, %s!":format("world"))
print("0123456789ABCDEF":sub(i, i))
```

## Motivation

Experienced Lua developers occasionally run into this paper-cut even after years of working with the language. Programmers in Lua frequently wants to format a user-facing message using a constant string, but the parser will not accept it as legible syntax.

## Design

Formally, the proposal is to move the `String` parser from `exp` to `prefixexp`:

```diff
  var ::= Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name 
- exp ::= nil | false | true | Number | String | `...´ | function |
+ exp ::= nil | false | true | Number | `...´ | function
        | prefixexp | tableconstructor | exp binop exp | unop exp
- prefixexp ::= var | functioncall | `(´ exp `)´
+ prefixexp ::= String | var | functioncall | `(´ exp `)´
  functioncall ::= prefixexp args | prefixexp `:´ Name args
```

By itself, this change introduces an additional ambiguity because of the combination of non-significant whitespace and function calls with string literal as the first argument without the use of parentheses.

Consider code like this:

```lua
local foo = bar
("fmt"):format(...)
```

The grammar for this sequence suggests that the above is a function call to bar with a single string literal argument, "fmt", and format method is called on the result. This is a consequence of line endings not being significant, but humans don't read the code like this, and are likely to think that here, format is called on the string literal "fmt".

Because of this, Lua 5.1 produces a syntax error whenever function call arguments start on the next line. Luau has the same error production rule; future versions of Lua remove this restriction but it's not clear that we want to remove this as this does help prevent errors.

The grammar today also allows calling functions with string literals as their first (and only) argument without the use of parentheses; bar "fmt" is a function call. This is helpful when defining embedded domain-specific languages.

By itself, this proposal thus would create a similar ambiguity in code like this:

```lua
local foo = bar
"fmt":format(...)
```

While we could extend the line-based error check to include function literal arguments, this is not syntactically backwards compatible and as such may break existing code. A simpler and more conservative solution is to disallow string literal as the leading token of a new statement - there are no cases when this is valid today, so it's safe to limit this.

Doing so would prohibit code like this:

```lua
"fmt":format(...)
```

However, there are no methods on the string object where code like this would be meaningful. As such, in addition to changing the grammar wrt string literals, we will add an extra ambiguity error whenever a statement starts with a string literal.

## Drawbacks

None known.

## Alternatives

The infallible parser could be mended in this exact scenario to report a more friendly error message. We decided not to do this because there is more value to gain by simply supporting the main proposal.
