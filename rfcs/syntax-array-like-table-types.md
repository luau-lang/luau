# Array-like table types

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add special syntax for array-like table types, `{ T }`

## Motivation

Luau supports annotating table types. Tables are quite complex beasts, acting as essentially an associative container mapping any value to any other value, and to make it possible to reason about them at type level we have a more constrained definition of what a table is:

- A table can contain a set of string keys with a specific type for each key
- A table can additionally have an "indexer" for a given key/value type, meaning that it acts as an associative container mapping keys of type K to values of type V

The syntax for this right now looks like this:

```
{ key1: Type1, key2: Type2, [KeyType]: ValueType }
```

This is an example of a hybrid table that has both an indexer and a list of specific key/value pairs.

While Luau technically doesn't support arrays, canonically tables with integer keys are called arrays, or, more precisely, array-like tables. Luau way to specify these is to use an indexer with a number key:

```
{ [number]: ValueType }
```

(note that this permits use of non-integer keys, so it's technically richer than an array).

As the use of arrays is very common - for example, many library functions such as `table.insert`, `table.find`, `ipairs`, work on array-like tables - Luau users who want to type-annotate their code have to use array-like table annotations a lot.

`{ [number]: Type }` is verbose, and the only alternative is to provide a slightly shorter generic syntax:

```
type Array<T> = { [number]: T }
```

... but this is necessary to specify in every single script, as we don't support preludes.

## Design

This proposal suggests adding syntactic sugar to make this less cumbersome:

```
{T}
```

This will be exactly equivalent to `{ [number]: T }`. `T` must be a type definition immediately followed by `}` (ignoring whitespace characters of course)

Conveniently, `{T}` syntax matches the syntax for arrays in Typed Lua (a research project from 2014) and Teal (a recent initiative for a TypeScript-like Lua extension language from 2020).

## Drawbacks

This introduces a potential ambiguity wrt a tuple-like table syntax; to represent a table with two values, number and a string, it's natural to use syntax `{ number, string }`; however, how would you represent a table with just one value of type number? This may seem concerning but can be resolved by requiring a trailing comma for one-tuple table type in the future, so `{ number, }` would mean "a table with one number", vs `{ number }` which means "an array-like table of numbers".

## Alternatives

A different syntax along the lines of `[T]` or `T[]` was considered and rejected in favor of the current syntax:

a) This allows us to, in the future - if we find a good workaround for b - introduce "real" arrays with a distinct runtime representation, maybe even starting at 0! (whether we do this or not is uncertain and outside of scope of this proposal)
b) Square brackets don't nest nicely due to Lua lexing rules, where [[foo]] is a string literal "foo", so with either syntax with square brackets array-of-arrays is not easy to specify
