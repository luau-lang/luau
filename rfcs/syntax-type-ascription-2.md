# Relaxing type assertions

## Summary

The way `::` works today is really strange.  The best solution we can come up with is to allow `::` to convert between any two types.

## Motivation

The current Luau implementation of the `::` operator is a little bit strange.  Due to an accident, it can only be used for downcasts and casts to `any`.

Because of this property, `::` works as users expect in a great many cases, but doesn't actually make a whole lot of sense when scrutinized.

```lua
local t = {x=0, y=0}

local a = t :: {x: number, y: number, z: number} -- OK
local a = t :: {x: number} -- Error: This is an upcast!
```

Originally, we intended for type assertions to only be useful for upcasts.  This would make it consistent with the way annotations work in OCaml and Haskell and would never break soundness.  However, users have yet to report this oddity!  It is working correctly for them!

From this, we conclude that users are actually much more interested in having a convenient way to write a downcast.  We should bless this use and clean up the rules so they make more sense.

## Design

I propose that we change the meaning of the `::` operator to permit coersion between any two types.  It officially becomes a second way, like `any`, to completely bypass the type system.

## Drawbacks

`::` was originally envisioned to be a way for users to make the type inference engine work smarter and better for them.  Users have taught us that they are more interested in downcasts and we should be responsive to that.

## Alternatives

We could simply update `::` to act the way it was originally envisioned.  `::` would not break soundness and users would have to figure out some other way to downcast.  Code they have already written to do so would break.  To be truly morally true to the original vision, we would have to also forbid `:: any`, which would be a further usability hit.

We could introduce yet another operator expressly for downcasts, but we'd have to come up with its name and explain the difference to end users.

Lastly, we could permit both upcasts and downcasts, but not permit casts between totally unrelated types.  I think this is doable, but potentially inefficient: Luau would basically have to pay the CPU cycles required to try both and rewind if they don't work.
