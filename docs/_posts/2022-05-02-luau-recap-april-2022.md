---
layout: single
title:  "Luau Recap: April 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-april-2022/).]

It's been a bit of a quiet month.  We mostly have small optimizations and bugfixes for you.

It is now allowed to define functions on sealed tables that have string indexers.  These functions will be typechecked against the indexer type.  For example, the following is now valid:

```lua
local a : {[string]: () -> number} = {}

function a.y() return 4 end -- OK
```

Autocomplete will now provide string literal suggestions for singleton types.  eg

```lua
local function f(x: "a" | "b") end
f("_") -- suggest "a" and "b"
```

Improve error recovery in the case where we encounter a type pack variable in a place where one is not allowed.  eg `type Foo<A...> = { value: A... }`

When code does not pass enough arguments to a variadic function, the error feedback is now better.

For example, the following script now produces a much nicer error message:
```lua
type A = { [number]: number }
type B = { [number]: string }

local a: A = { 1, 2, 3 }

-- ERROR: Type 'A' could not be converted into 'B'
--     caused by:
--       Property '[indexer value]' is not compatible. Type 'number' could not be converted into 'string'
local b: B = a
```

If the following code were to error because `Hello` was undefined, we would erroneously include the comment in the span of the error.  This is now fixed.
```lua
type Foo = Hello -- some comment over here
```

Fix a crash that could occur when strict scripts have cyclic require() dependencies.

Add an option to autocomplete to cause it to abort processing after a certain amount of time has elapsed.
