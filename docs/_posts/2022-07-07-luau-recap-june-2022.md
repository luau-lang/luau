---
layout: single
title:  "Luau Recap: June 2022"
---

Luau is our new language that you can read more about at [https://luau-lang.org](https://luau-lang.org).

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-june-2022/).]

# Lower bounds calculation

A common problem that Luau has is that it primarily works by inspecting expressions in your program and narrowing the _upper bounds_ of the values that can inhabit particular variables.  In other words, each time we see a variable used, we eliminate possible sets of values from that variable's domain.

There are some important cases where this doesn't produce a helpful result.  Take this function for instance:

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

This is fine, but things go sour when we see the line `return nil`.  Since we are always narrowing, we take from this line the judgement that the return type of the function is `nil`.  Since we have already concluded that the function must return `number`, Luau reports an error.

What we actually want to do in this case is to take these `return` statements as inferences about the _lower_ bound of the function's return type.  Instead of saying "this function must return values of type `nil`," we should instead say "this function may _also_ return values of type `nil`."

Lower bounds calculation does precisely this.  Moving forward, Luau will instead infer the type `number?` for the above function.

This does have one unfortunate consequence: If a function has no return type annotation, we will no longer ever report a type error on a `return` statement.  We think this is the right balance but we'll be keeping an eye on things just to be sure.

Lower-bounds calculation is larger and a little bit riskier than other things we've been working on so we've set up a beta feature in Roblox Studio to enable them.  It is called "Experimental Luau language features."

Please try it out and let us know what you think!

## Known bug

We have a known bug with certain kinds of cyclic types when lower-bounds calculation is enabled.  The following, for instance, is known to be problematic.

```lua
type T = {T?}? -- spuriously reduces to {nil}?
```

We hope to have this fixed soon.

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
