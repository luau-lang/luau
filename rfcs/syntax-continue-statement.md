# continue statement

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add `continue` statement to `for`, `while` and `repeat` loops using a context-sensitive keyword to preserve compatibility.

## Motivation

`continue` statement is a feature present in basically all modern programming languages. It's great for ergonomics - often you want the loop to only process items of a specific kind, so you can say `if item.kind ~= "blah" then continue end` in the beginning of the loop.

`continue` never makes code that was previously impossible to write possible, but it makes some code easier to write.

We'd like to add this to Luau but we need to keep backwards compatibility - all existing scripts that parse correctly must parse as they do now. The rest of the proposal outlines the exact syntax and semantics that makes it possible.

## Design

`continue` statement shall be the statement that *starts* with "continue" identifier (*NOT* keyword - effectively it will be a context-sensitive keyword), and such that the *next* token is none of (`.`, `[`, `:`, `{`, `(`, `=`, string literal or ',').

These rules effectively say that continue statement is the statement that *does not* parse as a function call or the beginning of an assignment statement.

This is a continue statement:

```
do
continue
end
```

This is not a continue statement:

```
do
continue = 5
end
```

This is not a continue statement:

```
do
continue(5)
end
```

This is not a continue statement either, why do you ask?

```
do
continue, foo = table.unpack(...)
end
```

These rules are simple to implement. In any Lua parser there is already a point where you have to disambiguate an identifier that starts an assignment statement (`foo = 5`) from an identifier that starts a function call (`foo(5)`). It's one of the few, if not the only, place in the Lua grammar where single token lookahead is not sufficient to parse Lua, because you could have `foo.bar(5)` or `foo.bar=5` or `foo.bar(5)[6] = 7`.

Because of this, we need to parse the entire left hand side of an assignment statement (primaryexp in Lua's BNF) and then check if it was a function call; if not, we'd expect it to be an assignment statement.

Alternatively in this specific case we could parse "continue", parse the next token, and if it's one of the exclusion list above, roll the parser state back and re-parse the non-continue statement. Our lexer currently doesn't support rollbacks but it's also an easy strategy that other implementations might employ for `continue` specifically.

The rules make it so that the only time we interpret `continue` as a continuation statement is when in the old Lua the program would not have compiled correctly - because this is not valid Lua 5.x:

```
do
continue
end
```

There is one case where this can create new confusion in the newly written code - code like this:

```
do
continue
(foo())(5)
end
```

could be interpreted both as a function call to `continue` (which it is!) and as a continuation statement followed by a function call (which it is not!). Programmers writing this code might expect the second treatment which is wrong.

We have an existing linter rule to prevent this, however *for now* we will solve this in a stronger way:

Once we parse `continue`, we will treat this as a block terminator - similarly to `break`/`return`, we will expect the block to end and the next statement will have to be `end`. This will make sure there's no ambiguity. We may relax this later and rely on the linter to tell people when the code is wrong.

Semantically, continue will work as you would expect - it would skip the rest of the loop body, evaluate the condition for loop continuation (e.g. check the counter value for numeric loops, call the loop iterator for generic loops, evaluate while/repeat condition for while/repeat loops) and proceed accordingly. Locals declared in the loop body would be closed as well.

One special case is the `until` expression: since it has access to the entire scope of `repeat` statement, using `continue` is invalid when it would result in `until` expression accessing local variables that are declared after `continue`.

## Drawbacks

Adding `continue` requires a context-sensitive keyword; this makes editor integration such as syntax highlighting more challenging, as you can't simply assume any occurrence of the word `continue` is referring to the statement - this is different from `break`.

Implementing `continue` requires special care for `until` statement as highlighted in the design, which may make compiler slower and more complicated.

## Alternatives

In later versions of Lua, instead of `continue` you can use `goto`. However, that changes control flow to be unstructured and requires more complex implementation and syntactic changes.
