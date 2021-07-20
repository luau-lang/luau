# Relaxing type assertions

## Summary

The way `::` works today is really strange.  The best solution we can come up with is to allow `::` to convert between any two types.

## Motivation

The current Luau implementation of the `::` operator is a little bit strange.  Due to an accident, it can only be used for downcasts and casts to `any`.

This is unfortunate because it works in many cases where users feel that it should, but doesn't actually make a whole lot of sense when scrutinized.

```lua
local t = {x=0, y=0}

local a = t :: {x: number, y: number, z: number} -- OK
local a = t :: {x: number} -- Error: This is an upcast!
```

Originally, we intended for type assertions to only be useful for upcasts.  This would make it consistent with the way annotations work in OCaml and Haskell and
would never break soundness.  However, users have yet to report this oddity!  It is working correctly for them!

We must conclude that users are not interested in explicit upcasting syntax.  Only downcasts and "samecasts" (a cast from a type to itself) are interesting.

## Design

I propose that we change the meaning of the `::` operator to permit _any coersion whatsoever_.  The `::` operator thus becomes morally equivalent to using `any`:
scripts that include either cannot be assumed to be sound.

## Drawbacks

`::` was originally envisioned to be a way for users to make the type inference engine work smarter and better for them.  Making all casts succeed
thoroughly puts an end to that goal.  This is acceptable because nobody seems to be interested in using it that way anyway.

## Alternatives

We could simply update `::` to act the way it was originally envisioned.  This would break some code in lua-apps, where it is expressly used to perform a downcast. This would satisfy this RFR author, but leaves open the problem of how to perform a downcast in Luau.  If we were to go ahead with this plan, programmers would have to write `(foo :: any) :: TargetType` to effect a downcast.

We could introduce yet another operator expressly for downcasts, but we'd have to come up with its name and explain the difference to end users.
