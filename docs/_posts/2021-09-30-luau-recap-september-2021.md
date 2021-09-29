---
layout: single
title:  "Luau Recap: September 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-september-2021/).]

## Generic functions

The big news this month is that generic functions are back!

Luau has always supported type inference for generic functions, for example:
```lua
type Point<X,Y> = { x: X, y: Y }
function swap(p)
  return { x = p.y, y = p.x }
end
local p : Point<number, string> = swap({ x = "hi", y = 37 })
local q : Point<boolean, string> = swap({ x = "hi", y = true })
```
but up until now, there's been no way to write the type of `swap`, since Luau didn't have type parameters to functions (just regular old data parameters). Well, now you can:
```lua
function swap<X, Y>(p : Point<X, Y>): Point<Y, X>
  return { x = p.y, y = p.x }
end
```
Generic functions can be used in function declarations, and function types too, for example
```lua
type Swapper = { swap : <X, Y>(Point<X, Y>) -> Point<Y, X> }
```

People may remember that back in
[April](https://devforum.roblox.com/t/luau-recap-april-2021/) we
announced generic functions, but then had to disable them. That was
because [DataBrain](https://devforum.roblox.com/u/databrain) discovered a [nasty
interaction](https://devforum.roblox.com/t/recent-type-system-regressions-for-generic-parametered-functions/)
between `typeof` and generics, which meant that it was possible to
write code that needed nested generic functions, which weren't
supported back then.

Well, now we do support nested generic functions, so you can write code like
```lua
function mkPoint(x)
  return function(y)
    return { x = x, y = y }
  end
end
```
and have Luau infer a type where a generic function returns a generic function
```lua
function mkPoint<X>(x : X) : <Y>(Y) -> Point<X,Y>
  return function<Y>(y : Y) : Point<X,Y>
    return { x = x, y = y }
  end
end
```
For people who like jargon, Luau now supports *Rank N Types*, where
previously it only supported Rank 1 Types.

## Bidirectional typechecking

Up until now, Luau has used *bottom-up* typechecking. For example, for
a function call `f(x)` we first find the type of `f` (say it's
`(T)->U`) and the type for `x` (say it's `V`), make sure that `V` is
a subtype of `T`, so the type of `f(x)` is `U`.

This works in many cases, but has problems with examples like registering
callback event handlers. In code like
```lua
part.Touched:Connect(function (other) ... end)
```
if we try to typecheck this bottom-up, we have a problem because
we don't know the type of `other` when we typecheck the body of the function.

What we want in this case is a mix of bottom-up and *top-down* typechecking.
In this case, from the type of `part.Touched:Connect` we know that `other` must
have type `BasePart`.

This mix of top-down and bottom-up typechecking is called
*bidirectional typechecking*, and means that tools like type-directed
autocomplete can provide better suggestions.

## Editor features

We have made some improvements to the Luau-powered autocomplete beta feature in Roblox Studio:

 * We no longer give autocomplete suggestions for client-only APIs in server-side scripts,
   or vice versa.
 * For table literals with known shape, we provide autocomplete suggestions for properties.
 * We provide autocomplete suggestions for `Player.PlayerGui`.
 * Keywords such as `then` and `else` are autocompleted better.
 * Autocompletion is disabled inside a comment span (a comment starting `--[[`).

## Typechecking improvements

In other typechecking news:

 * The Luau constraint resolver can now refine the operands of equality expressions.
 * Luau type guard refinements now support more arbitrary cases, for instance `typeof(foo) ~= "Instance"` eliminates anything not a subclass of `Instance`.
 * We fixed some crashes caused by use-after-free during type inference.
 * We do a better job of tracking updates when script is moved inside the data model.
 * We fixed one of the ways that [recursive types could cause free types to leak](https://devforum.roblox.com/t/free-types-leaked-into-this-modules-public-interface/1459070).
 * We improved the way that `return` statements interact with mutually recursive
   function declarations.
 * We improved parser recovery from code which looks like a function call (but isn't) such as
```lua
local x = y
(expr)[smth] = z
```
 * We consistently report parse errors before type errors.
 * We display more types as `*unknown*` rather than as an internal type name like `error####`.
 * Luau now infers the result of `Instance:Clone()` much more accurately.

## Performance improvements

 * `Vector3.new` constructor has been optimized and is now ~2x faster
 * A previously implemented optimization for table size prediction has been enhanced to predict final table size when `setmetatable` is used, such as `local self = setmetatable({}, Klass)`
 * Method calls for user-specified objects have been optimized and are now 2-4% faster
 * `debug.traceback` is now 1.5x faster, although `debug.info` is likely still superior for performance-conscious code
 * Creating table literals with explicit numeric indices, such as `{ [1] = 42 }`, is now noticeably faster, although list-style construction is still recommended.

## Other improvements

 * The existing 'TableLiteral' lint now flags cases when table literals have duplicate numeric indices, such as `{ [1] = 1, [1] = 2 }`
