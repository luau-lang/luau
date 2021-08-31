---
layout: single
title:  "Luau Recap: August 2021"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-august-2021/).]

## Editor features

The Roblox Studio [Luau-Powered Autocomplete & Language Features Beta](https://devforum.roblox.com/t/script-editor-luau-powered-autocomplete-language-features-beta) that our team has been working on has finally been released!
Be sure to check that out and leave your feedback for things we can improve.

To support that feature, a lot of work went into:
* Improving fault-tolerant parser recovery scenarios
* Storing additional information in the AST, including comments, better location information and partial syntax data
* Tracking additional information about types and their fields, including tracking definition locations, function argument names, deprecation state and custom Roblox-specific tags
* Updating reflection information to provide more specific `Instance` types and correct previously missing or wrong type annotations
* Hybrid typechecking mode which tries to infer types even in scripts with no typechecking enabled
* Support for types that are attached to the `DataModel` tree elements to provide instance member information
* Placing limits to finish typechecking in a finite space/time
* Adding Autocomplete API for the Roblox Studio to get location-based entity information and appropriate suggestions
* Additional type inference engine improvements and fixes

While our work continues to respond to the feedback we receive, our team members are shifting focus to add generic functions, improve type refinements in conditionals, extend Parallel Luau, improve Lua VM performance and provide documentation. 

## Typechecking improvements

Type constraint resolver now remembers constraints placed on individual table fields.

This should fix false-positive errors reported after making sure the optional table field is present:
```lua
--!strict
local t: {value: number?} = {value = 2}

if t.value then
    local v: number = t.value -- ok
end
```

And it can also refine field type to a more specific one:
```lua
--!strict
local t: {value: string|number} = {value = 2}

if type(t.value) == "number" then
    return t.value * 2 -- ok
end
```

Like before, combining multiple conditions using 'and' and 'not' is also supported.

---

Constructing arrays with different values for optional/union types are now also supported for individual table fields and in functions call arguments:
```lua
--!strict
type Foo = {x: number | string, b: number?}

local function foo(l: {Foo}) end

foo({
	{x = 1234567},
	{x = "hello"}, -- now ok
})

type Bar = {a: {Foo}}

local foos: Bar = {a = {
	{x = 1234567},
	{x = "hello", b = 2}, -- now ok
}}
```

---

Finally, we have fixed an issue with Roblox class field access using indexing like `part["Anchored"] = true`.

## Linter improvements

We have added a new linter check for duplicate local variable definitions.

It is created to find duplicate names in cases like these:
```lua
local function foo(a1, a2, a2) -- Function argument 'a2' already defined on column 24
local a1, a2, a2 = f() -- Variable 'a2' already defined on column 11

local bar = {}
function bar:test(self) -- Function argument 'self' already defined implicitly
```

Our UnknownType linter warning was extended to check for correct class names passed into `FindFirstChildOfClass`, `FindFirstChildWhichIsA`, `FindFirstAncestorOfClass` and `FindFirstAncestorWhichIsA` functions.

## Performance improvements

We have added an optimization to 'table.unpack' for 2x performance improvement.

We've also implemented an extra optimization for tables to predict required table capacity based on fields that are assigned to it in the code after construction. This can reduce the need to reallocate tables.

Variadic call performance was fine-tuned and is now ~10% faster.

Construction of array literals was optimized for a ~7% improvement.

Another optimization this month changes the location and rate of garbage collection invocations.
We now try to avoid calling GC during the script execution and perform all the work in the GcJob part of the frame (it could be seen in the performance profiler). When possible, we can now skip that job in the frame completely, if we have some memory budget available.

## Other improvements

For general stability improvements we fixed a crash when strange types like '`nil??`' are used and when users have their own global functions named '`require`'.

Indexing a table with an incompatible type will now show squiggly error lines under the index instead of the whole expression, which was a bit misleading.

An issue with debug information that caused `repeat ... until` condition line to be skipped when stepping was fixed.

Type output was improved to replace display of types like '`{<g405>(g405) -> g405}`' with '`{<a>(a) -> a}`'.
