---
layout: single
title:  "Luau Recap: June 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-june-2022/).]

# Lower bounds calculation

A common problem that Luau has is that it primarily works by inspecting expressions in your program and narrowing the _upper bounds_ of the values that can inhabit particular variables.  In other words, each time we see a variable used, we eliminate possible sets of values from that variable's domain.

There are lots of cases where this isn't actually the best thing.  Take this function for instance:

```lua
function find_first_if(vec, f)
	for i, e in ipairs(vec) do
		if f(e) then
			return i
		end
	end

	return nil
end
```

Luau scans the function from top to bottom and first sees the line `return i`.  It draws from this the inference that `find_first_if` must return the type of `i`, namely `number`.

This is fine, but we then see the line `return nil` and this is where things go sour.  Since we are always narrowing, we take from this line the assumption that the return type of the function is `nil`.  Unfortunately, we already think that the function must return `number` so we report an error.

What we actually want to do in this case is to take these `return` statements as inferences about the _lower_ bound of the function's return type.  Instead of saying "this function must return values of type `nil`," we should instead say "this function may _also_ return values of type `nil`."

Lower bounds calculation does precisely this.  Moving forward, Luau will instead infer the type `number?` for the above function.

This does have one unfortunate consequence: If a function has no return type annotation, we will no longer ever report a type error on a `return` statement.  We think this is the right balance, but we'll be keeping an eye on things just to be sure.

# Shared `self` Types for Objects

Another problem that has plagued us for quite some time is how exactly we should do type inference of OO patterns.  The following has been a constant thorn in our side.  Consider the following example:

```lua
local T = {}

function T:one()
    self:two()
end

function T:two()
    if true then
        self:one()
    else
        self:three()
    end
end

function T:three()
    self:two()
end
```

To the casual eye, this should be simple to infer, but there are technicalities that can throw a wrench in the works.  First off, it is allowed to explicitly pass a `self` argument to any of these methods.  eg `T.two(x)`  Luau takes this into account when inferring types, but doing so forces us to produce a very unfortunate type for this code:

```lua
{
    one: <a...>(t1) -> (),
    three: <b...>(t3) -> (),
    two: <c..., d...>(t2) -> ()
}
where
    t1 = {+
        two: (t1) -> (a...) 
    +};
    t2 = {+
        one: (t2) -> (c...),
        three: (t2) -> (d...)
    +};
    t3 = {+
        two: (t3) -> (b...)
    +}
```

We can see here that Luau is inferring a distinct `self` type for each method, but some of those `self`s themselves call other methods, which gives us these extra incomplete images of the intended type.  In real, nontrivial code, this pattern can quickly cause the size of a class's type to balloon out of control.  This results in confusing error messages and even performance bottlenecks in the type checker.

Moving forward, we are going to take a slightly more opinionated stance on this use case by having Luau assume that every method on a table (that is, every function declared with the syntax `function A:b`) takes the same `self` type as every other method.

Luau will now infer a much nicer type for this code:

```lua
{
    one: <a..., b..., c...>(self: t1) -> (),
    three: <a..., b..., c...>(self: t1) -> (),
    two: <a..., b..., c...>(self: t1) -> ()
} where t1 = {+
    one: (t1) -> (a...),
    three: (t1) -> (b...),
    two: (t1) -> (c...)
+}
```

We still have one duplicated table type to sort out on our end, but this is clearly much closer to what the author intended.

## Beta Feature

Shared-self and lower-bounds calculation are larger and a little bit riskier than other things we've been working on, so we've set up a beta feature in Roblox Studio to enable them.  It is called "Experimental Luau language features."

Please try them out and let us know what you think!

# All table literals now result in unsealed tables

Previously, the only way to create a sealed table was by with a literal empty table.  We have relaxed this somewhat: Any table created by a `{}` expression is considered to be unsealed within the scope where it was created:

```lua
local T = {}
T.x = 5 -- OK

local V = {x=5}
V.y = 2 -- previously disallowed.  Now OK.

function mkTable()
    return {x = 5}
end

local U = mkTable()
U.y = 2 -- Still disallowed: U is sealed
```

# Other fixes

* Adjust indentation and whitespace when creating multiline string representations of types, resulting in types that are easier to read.
* Some small bugfixes to autocomplete
* Fix a case where accessing a nonexistent property of a table would not result in an error being reported.
* Improve parser recovery for the incorrect code `function foo() -> ReturnType` (the correct syntax is `function foo(): ReturnType`)
* Improve the parse error offered for code that improperly uses the `function` keyword to start a type eg `type T = function`
* Some small crash fixes and performance improvements

# Thanks!

A very special thanks to all of our open source contributors:

* [Allan N Jeremy](https://github.com/AllanJeremy)
* [Daniel Nachun](https://github.com/danielnachun)
* [JohnnyMorganz](https://github.com/JohnnyMorganz/)
* [Petri Häkkinen](https://github.com/petrihakkinen)
* [Qualadore](https://github.com/Qualadore)
