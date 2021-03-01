---
layout: single
title:  "Luau Recap: October 2020"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau); we’ve been so busy working on the current projects that we didn’t do an update in September, so let’s look at changes that happened since August!

Many people work on these improvements, with the team slowly growing - thanks @Apakovtac, @EthicalRobot, @fun_enthusiast, @machinamentum, @mrow_pizza and @zeuxcg!

[Originally posted on the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-october-2020/).]

## Types are very close

We’ve been in beta for a while now, but we’re steadily marching towards getting the first release of the type checker, what we call “types v0”, out of the door. It turns out that we’ve substantially underestimated the effort required to make the type system robust, strike the balance between “correct” and “usable” and give quality diagnostics in the event we do find issues with your code 🙂

Because of this, we’re changing the original plans for the release a bit. We’re actively working on a host of changes that we consider to be part of the “v0” effort, and when they are all finished - which should happen next month, fingers crossed - we’re going to be out of beta!

However, by default, on scripts with no annotations, we won’t actually activate type checking. You would have to opt into the type checking by using `--!nonstrict` or `--!strict`, at the top of each script. We are also going to open the second beta, “All scripts use non-strict mode by default” or something along these lines.

This is important because we found that our non-strict mode still needs some more work to be more tolerant to some code that occurs commonly in Roblox and is correct, but doesn’t type-check. We’re going to evaluate what changes specifically are required to make this happen, but we didn’t want the extra risk of a flood of reports about issues reported in existing code to shift the release date in an unpredictable fashion.

To that end, we’ve been working on Lots and Lots and Lots and Lots and Lots of changes to finish the first stage. Some of these changes are already live and some are rolling out; the amount of changes is so large that I can’t possibly list the up-to-date status on each one as these recaps are synthesized by the human who is writing this on a Friday night, so here’s just a raw list of changes that may or may not have been enabled:

 * Strict mode is now picky about passing extra arguments to functions, even though they are discarded silently at runtime, as this can hide bugs
 * The error message about using a : vs . during type checking is now much more precise
 * Recursive type annotations shouldn’t crash the type checker now, and we limit the recursion and iteration depth during type checking in a few cases in general in an effort to make sure type checker always returns in finite time
 * Binary relational operators (`<` et al) are now stricter about the argument types and infer the argument types better
 * Function argument and return types are now correctly contra- and co-variant; if this looks like gibberish to you, trust me - it’s for the best!
 * Fixed a few problems with indexing unions of tables with matching key types
 * Fixed issues with tracing types across modules (via require) in non-strict mode
 * Error messages for long table types are now trimmed to make the output look nicer
 * Improve the interaction between table types of unknown shape (`{ [string]: X }`) and table types of known shape.
 * Fix some issues with type checking table assignments
 * Fix some issues with variance of table fields
 * Improve the legibility of type errors during function calls - errors now point at specific arguments that are incorrect, and mismatch in argument count should clearly highlight the problem
 * Fix types for many builtins including `ipairs`, `table.create`, `Color3.fromHSV`, and a few others
 * Fix missing callbacks for some instance types like `OnInvoke` for bindables (I think this one is currently disabled while we’re fixing a semi-related bug, but should be enabled soon!)
 * Rework the rules under which globals are okay to use in non-strict mode to mostly permit valid scripts to type-check; strict mode will continue to frown upon the use of global variables
 * Fix a problem with the beta where two scripts with identical names would share the set of errors/warnings, resulting in confusing error highlights for code that doesn’t exist
 * Improve the legibility of type errors when indexing a table without a given key
 * Improve the parsing error when trying to return a tuple; `function f(): string, number` is invalid since the type list should be parenthesized because of how our type grammar is currently structured
 * Type checker now clearly reports cases where it finds a cyclic dependency between two modules
 * Type errors should now be correctly sorted in the Script Analysis widget
 * Error messages on mismatches between numbers of values in return statements should now be cleaner, as well as the associated type mismatch errors
 * Improve error messages for comparison operators
 * Flag attempts to require a non-module script during type checking
 * Fix some cases where a type/typeof guard could be misled into inferring a non-sensible type
 * Increase the strictness of return type checks in strict mode - functions now must conform to the specified type signature, whereas before we’d allow a function to return no values even in strict mode
 * Improve the duplicate definition errors to specify the line of the first definition
 * Increase the strictness of binary operators in strict mode to enforce the presence of the given operator as a built-in or as part of the metatable, to make sure that strict mode doesn’t infer types when it can’t guarantee correctness
 * Improve the type errors for cyclic types to make them more readable
 * Make type checker more friendly by rewording a lot of error messages
 * Fix a few crashes in the type checker (although a couple more remain - working on them!)
 * … I think that’s it?
 * …edit ah, of course I forgot one thing - different enums that are part of the Roblox API now have distinct types and you can refer to the types by name e.g. `Enum.Material`; this should go live next week though.
If you want to pretend that you’ve read and understood the entire list above, just know that we’ve worked on making sure strict mode is more reliably reporting type errors and doesn’t infer types incorrectly, on making sure non-strict mode is more forgiving for code that is probably valid, and on making the type errors more specific, easier to understand, and correct.

## Type syntax changes

There’s only two small changes here this time around - the type syntax is now completely stable at this point, and any existing type annotation will continue parsing indefinitely. We of course reserve the right to add new syntax that’s backwards compatible :slight_smile:

On that note, one of the small changes is that we’ve finally removed support for fat arrows (`=>`); we’ve previously announced that this would happen and that thin arrows (`->`) are the future, and had warnings issued on the legacy syntax for a while. Now it’s gone.

On a positive note, we’ve added a shorter syntax for array-like table types. Whereas before you had to use a longer `{ [number]: string }` syntax to declare an array-like table that holds strings, or had to define an `Array` type in every. single. module. you. ever. write. ever., now you can simply say `{string}`! This syntax is clean, in line with the value syntax for Lua table literals, and also was chosen by other research projects to add type annotations to Lua.

(if you’re a monster that uses mixed tables, you’ll have to continue using the longer syntax e.g. `{ [number]: string, n: number }`)

## Library changes

There’s only a few small tweaks here this time around on the functionality front:

 * `utf8.charpattern` is now exactly equal to the version from Lua 5.3; this is now possible because we support `\0` in patterns, and was suggested by a user on devforum. We do listen!
 * `string.pack` now errors out early when the format specifier is Way Too Large. This was reported on dev forum and subsequently fixed. Note that trying to generate a Moderately Large String (like, 100 MB instead of 100 GB) will still succeed but may take longer than we’d like - we have a plan to accelerate operations on large strings substantially in the coming months.
 
## Performance improvements

We were super focused on other things so this is very short this time around. We have a lot of ideas here but they are waiting for us to finish some other large projects!

 * Method calls on strings via `:` are now ~10% faster than before. We still recommend using fully-qualified calls from string library such as `string.foo(str)`, but extra performance never hurts!
 * Speaking of string methods, string.sub is now ~20% faster than before with the help of voodoo magic.

## Miscellaneous fixes

There were a few small fixes that didn’t land into any specific category that I wanted to highlight:

 * In some rare cases, debug information on conditions inside loops have been fixed to stop debugger from incorrectly suggesting that the current line is inside a branch that wasn’t taken. As usual, if you ever see debugger misbehaving, please file bugs on this!
 * Code following `assert(false)` is now treated as an unreachable destination from the linting and type checking point of view, similarly to error calls.
 * Linting support for various format strings has been greatly improved based on fantastic feedback from @Halalaluyafail3 (thanks!).
 
Ok, phew, that’s what I get for skipping a month again. Please don’t hesitate to report bugs or suggestions, here or via separate posts. Due to our usual end-of-year code freeze there’s going to be one more recap at the end of the year where we will look back at 2020 and take a small peek into the future.

