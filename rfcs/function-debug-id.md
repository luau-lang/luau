# `debug.id`

## Summary

Add `debug.id` which accepts a value and always returns the identifier unique to that value.

## Motivation

When debugging, it is often useful to compare identity. Usually `print` will print the memory address of the table or userdata, but when `__tostring` is overriden, the address becomes unobtainable.

`rawequal` can compare equality between two arguments, but is not suitable for a human comparing output between two `print` calls or between keys or values in a table.

## Design

`debug.id(value: string | userdata | table | thread | function): number` always returns the identifier unique to that value, and is not possible to override. The identifier can be reused after the value associated with it is garbage collected.

While tables and userdata have individualized metatables, also allowing functions and threads to be passed to `debug.id` would let their type-level `__tostring` be redefined. Strings are useful to accept because of long, similar strings.

## Drawbacks

Identifiers being reused has the potential to confuse humans reading them.

## Alternatives

Lua has `string.format("%p", {})`, but this is an invalid format option in Luau and less obvious.

If not implemented, it remains annoying to compare identity when dealing with tables or userdata that override `__tostring`.