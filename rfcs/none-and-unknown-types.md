# Add none and unknown types

## Summary

Add top and bottom types that ane inhabited by everything and nothing respectively.

## Motivation

There are lots of cases in local type inference, semantic subtyping,
and type normalization, where it would be useful to have top and
bottom types. Currently, `any` is filling that role, but it has
special "switch off the type system" superpowers.

## Design

Add:

* a type `none`, inhabited by nothing, and
* a type `unknown`, inhabited everything.

Use them, rather than `any` where appropriate. Ideally, we would then never infer `any`.

## Drawbacks

Another bit of complexity budget spent.

These types will be visible to creators, so yay bikeshedding!

## Alternatives

Stick with the current use of `any` for these cases.
