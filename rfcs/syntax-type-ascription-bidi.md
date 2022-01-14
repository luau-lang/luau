# Relaxing type assertions

## Summary

The way `::` works today is really strange.  The best solution we can come up with is to allow `::` to convert between any two related types.

**Status**: Implemented

## Motivation

Due to an accident of the implementation, the Luau `::` operator can only be used for downcasts and casts to `any`.

Because of this property, `::` works as users expect in a great many cases, but doesn't actually make a whole lot of sense when scrutinized.

```lua
local t = {x=0, y=0}

local a = t :: {x: number, y: number, z: number} -- OK
local a = t :: {x: number} -- Error: This is an upcast!
```

Originally, we intended for type assertions to only be useful for upcasts.  This would make it consistent with the way annotations work in OCaml and Haskell and would never break soundness.  However, users have yet to report this oddity!  It is working correctly for them!

From this, we conclude that users are actually much more interested in having a convenient way to write a downcast.  We should bless this use and clean up the rules so they make more sense.

## Design

I propose that we change the meaning of the `::` operator to permit conversions between any two types for which either is a subtype of the other.

## Drawbacks

`::` was originally envisioned to be a way for users to make the type inference engine work smarter and better for them.  The fact of the matter is, though, that downcasts are useful to our users.  We should be responsive to that.

## Alternatives

We initially discussed allowing `::` to coerce anything to anything else, acting as a full bypass of the type system.  We are not doing this because it is really just not that hard to implement: All we need to do is to succeed if unification works between the two types in either direction. Additionally, requiring one type to be subtype of another catches mistakes when two types are completely unrelated, e.g. casting a `string` to a table will still produce an error when this proposal is in effect - this will make sure that `::` is as safe of a bypass as it can be in practice.
