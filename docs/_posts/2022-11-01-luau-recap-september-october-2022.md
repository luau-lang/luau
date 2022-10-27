---
layout: single
title:  "Luau Recap: September & October 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-september-october-2022/).]

## Semantic subtyping

One of the most important goals for Luau is to avoid *false
positives*, that is cases where Script Analysis reports a type error,
but in fact the code is correct. This is very frustrating, especially
for beginners. Spending time chasing down a gnarly type error only to
discover that it was the type system that's wrong is nobody's idea of fun!

We are pleased to announce that a major component of minimizing false
positives has landed, *semantic subtyping*, which removes a class of false positives caused
by failures of subtyping.  For example, in the program

```lua
  local x : CFrame = CFrame.new()
  local y : Vector3 | CFrame
  if (math.random()) then
    y = CFrame.new()
  else
    y = Vector3.new()
  end
  local z : Vector3 | CFrame = x * y -- Type Error!
```

an error is reported, even though there is no problem at runtime. This
is because `CFrame`'s multiplication has two overloads:

```lua
    ((CFrame, CFrame) -> CFrame)
  & ((CFrame, Vector3) -> Vector3)
```

The current syntax-driven algorithm for subtyping is not sophisticated
enough to realize that this is a subtype of the desired type:

```lua
  (CFrame, Vector3 | CFrame) -> (Vector3 | CFrame)
```

Our new algorithm is driven by the semantics of subtyping, not the syntax of types,
and eliminates this class of false positives.

If you want to know more about semantic subtyping in Luau, check out our
[technical blog post](https://luau-lang.org/2022/10/31/luau-semantic-subtyping.html)
on the subject.

## Other analysis improvements

* Improve stringification of function types.
* Improve parse error warnings in the case of missing tokens after a comma.
* Improve typechecking of expressions involving variadics such as `{ ... }`.
* Make sure modules don't return unbound generic types. 
* Improve cycle detection in stringifying types.
* Improve type inference of combinations of intersections and generic functions.
* Improve typechecking when calling a function which returns a variadic e.g. `() -> (number...)`.
* Improve typechecking when passing a function expression as a parameter to a function.
* Improve error reporting locations.
* Remove some sources of memory corruption and crashes.

## Other runtime and debugger improvements

* Improve performance of accessing debug info.
* Improve performance of `getmetatable` and `setmetatable`.
* Remove a source of freezes in the debugger.
* Improve GC accuracy and performance.

## Thanks

Thanks for all the contributions!

* [AllanJeremy](https://github.com/AllanJeremy)
* [JohnnyMorganz](https://github.com/JohnnyMorganz)
* [jujhar16](https://github.com/jujhar16)
* [petrihakkinen](https://github.com/petrihakkinen)
