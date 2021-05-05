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

The recommendation is that we should keep statements starting with string tokens as illegal syntax, as it is too niche to support use cases with side-effecting functions.

## Drawbacks

Statements starting by parsing `prefixexp` will now allow string tokens to be parsed despite that the return values of the function calls are always discarded. This is a non-issue if we obey the recommendation to ban statements starting with string tokens.

## Alternatives

The infallible parser could be mended in this exact scenario to report a more friendly error message. We decided not to do this because there is more value to gain by simply supporting the main proposal.
