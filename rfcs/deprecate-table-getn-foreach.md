# Deprecate table.getn/foreach/foreachi

**Status**: Implemented

## Summary

Mark table.getn/foreach/foreachi as deprecated

## Motivation

`table.getn`, `table.foreach` and `table.foreachi` were deprecated in Lua 5.1 that Luau is based on, and removed in Lua 5.2.

`table.getn(x)` is equivalent to `rawlen(x)` when `x` is a table; when `x` is not a table, `table.getn` produces an error. It's difficult to imagine code where `table.getn(x)` is better than either `#x` (idiomatic) or `rawlen(x)` (fully compatible replacement). However, `table.getn` is slower and provides yet another way to perform an operation, leading new users of the language to use it unknowingly.

`table.foreach` is equivalent to a `for .. pairs` loop; `table.foreachi` is equivalent to a `for .. ipairs` loop; both may also be replaced by generalized iteration. Both functions are significantly slower than equivalent `for` loop replacements, are more restrictive because the function can't yield, and result in new users (particularly coming from JS background) unknowingly using these thus producing non-idiomatic non-performant code.

In both cases, the functions bring no value over other library or language alternatives, and thus just serve as a distraction.

## Design

We will mark all three functions as deprecated. The only consequence of this change is that the linter will start emitting warnings when they are used.

Removing support for these functions doesn't provide any measurable value and as such is not planned in the foreseeable future because it may cause backwards compatibility issues.

## Drawbacks

None

## Alternatives

If we consider table.getn/etc as supported, we'd want to start optimizing their usage which gets particularly tricky with foreach and requires more compiler machinery than this is probably worth.
