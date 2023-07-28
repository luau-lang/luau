---
layout: single
title:  "Luau Recap: July 2023"
---

Our team is still spending a lot of time working on upcoming replacement for our type inference engine as well as working on native code generation to improve runtime performance.

But doing that doesn't mean we can't have a recap of other things we and our contributors did, even if it's only a small list.

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-july-2023/).]

## Analysis improvements

Indexing table intersections using `x["prop"]` syntax has been fixed and no longer reports a false positive error:

```lua
type T = { foo: string } & { bar: number }
local x: T = { foo = "1", bar = 2 }

local y = x["bar"] -- This is no longer an error
```

Generic `T...` type is now convertible to `...any` variadic parameter.

This solves issues people had with variadic functions and variadic argument:

```lua
local function foo(...: any)
    print(...)
end

local function bar<T...>(...: T...)
    foo(...) -- This is no longer an error
end
```

We have also improved our general typechecking performance by ~17% and by additional ~30% in modules with complex types.

Other fixes include:

* fixed issue with type `T?` not being convertible to `T | T` or `T?` which could've generated confusing errors
* `os.date` return type is now inferred as `DateTypeResult` when argument is "*t" or "!*t"

## Runtime improvements

Out-of-memory exception handling has been improved.
`xpcall` handlers will now actually be called with a "not enough memory" string and whatever string/object they return will be correctly propagated.

Other runtime improvements we've made:

* table.sort was improved further. It now guarantees N*log(N) time complexity in the worst case
* inlining of functions is now possible even when they used to compute their own arguments

## Autocomplete improvements

An issue with exported types not being suggested is now fixed.

## Debugger improvements

We have fixed search for closest executable breakpoint line.

Previously, breakpoints might have been skipped in `else` blocks at the end of a function.
This simplified example shows the issue:

```lua
local function foo(isIt)
    if isIt then
        print("yes")
    else
        -- When 'true' block exits the function, breakpoint couldn't be placed here
        print("no")
    end
end
```

## Thanks

A very special thanks to all of our open source contributors:

* [Petri Häkkinen](https://github.com/petrihakkinen)
* [JohnnyMorganz](https://github.com/JohnnyMorganz)
* [Gael](https://github.com/TheGreatSageEqualToHeaven)
* [Jan](https://github.com/Jan200101)
* [Alex Orlenko](https://github.com/khvzak)
* [mundusnine](https://github.com/mundusnine)
* [Ben Mactavsin](https://github.com/BenMactavsin)
* [RadiatedExodus](https://github.com/RealEthanPlayzDev)
* [Lodinu Kalugalage](https://github.com/imlodinu)
* [MagelessMayhem](https://github.com/MagelessMayhem)
* [Someon1e](https://github.com/Someon1e)
