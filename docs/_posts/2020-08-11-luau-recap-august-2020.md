---
layout: single
title:  "Luau Recap August 2020"
---

As everyone knows by now, Luau is our new language stack that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau) and the month following June is August so let’s talk about changes, big and small, that happened since June!

Many people work on these improvements, with the team slowly growing - thanks @Apakovtac, @EthicalRobot, @fun_enthusiast, @mrow_pizza and @zeuxcg!

[Originally posted on the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-august-2020/).]

## Type annotations are safe to use in production!

When we started the Luau type checking beta, we’ve had a big warning sign in the post saying to not publish the type-annotated scripts to your production games which some of you did anyway. This was because we didn’t want to commit to specific syntax for types, and were afraid that changing the syntax would break your games.

This restriction is lifted now. All scripts with type annotations that parse & execute will continue to parse & execute forever. Crucially, for this to be true you must not be using old fat arrow syntax for functions, which we warned you about for about a month now:

![Fat arrow deprecated]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-august-2020-arrow.png)

… and must not be using the `__meta` property which no longer holds special meaning and we now warn you about that:

![meta deprecated]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-august-2020-meta.png)

Part of the syntax finalization also involved changing the precedence on some type annotations and adding support for parentheses; notably, you can now mix unions and intersections if you know what that means (`(A & B) | C` is valid type syntax). Some complex type annotations changed their structure because of this - previously `(number) -> string & (string) -> string` was a correct way to declare an intersection of two function types, but now to keep it parsing the same way you need to put each function type in parentheses: `((number) -> string) & ((string) -> string)`.

Type checking is not out of beta yet - we still have some work to do on the type checker itself. The items on our list before going out of beta right now include:

 * Better type checking for unary/binary operators
 * Improving error messages to make type errors more clear
 * Fixing a few remaining crashes for complex scripts
 * Fixing conflation of warnings/errors between different scripts with the same path in the tree
 * Improving type checking of globals in nonstrict mode (strict mode will continue to frown upon globals)
 
Of course this doesn’t mark the end of work on the feature - after type checking goes out of beta we plan to continue working on both syntax and semantics, but that list currently represents the work we believe we have left to do in the first phase - please let us know if there are other significant issues you are seeing with beta beyond future feature requests!

## Format string analysis

A few standard functions in Luau are using format strings to dictate the behavior of the code. There’s `string.format` for building strings, `string.gmatch` for pattern matching, `string.gsub`'s replacement string, `string.pack` binary format specification and `os.date` date formatting.

In all of these cases, it’s important to get the format strings right - typos in the format string can result in unpredictable behavior at runtime including errors. To help with that, we now have a new lint rule that parses the format strings and validates them according to the expected format.

![String format]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-august-2020-format.png)

Right now this support is limited to direct library calls (`string.format("%.2f", ...)` and literal strings used in these calls - we may lift some of these limitations later to include e.g. support for constant locals.

Additionally, if you have type checking beta enabled, string.format will now validate the argument types according to the format string to help you get your `%d`s and `%s`es right.

![String format]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-august-2020-format2.png)

## Improvements to string. library

We’ve upgraded the Luau string library to follow Lua 5.3 implementation; specifically:

 * `string.pack/string.packsize/string.unpack` are available for your byte packing needs
 * `string.gmatch` and other pattern matching functions now support `%g` and `\0` in patterns
 
This change also [inadvertently] makes `string.gsub` validation rules for replacement string stricter - previously `%` followed by a non-digit character was silently accepted in a replacement string, but now it generates an error. This accidentally broke our own localization script [Purchase Prompt broken in some games (% character in title)](https://devforum.roblox.com/t/purchase-prompt-broken-in-some-games-character-in-title/686237)), but we got no other reports, and this in retrospect is a good change as it makes future extensions to string replacement safe… It was impossible for us to roll the change back and due to a long release window because of an internal company holiday we decided to keep the change as is, although we’ll try to be more careful in the future.

On a happier note, string.pack may seem daunting but is pretty easy to use to pack binary data to reduce your network traffic (note that binary strings aren’t safe to use in DataStores currently); I’ve posted an example in the release notes thread [Release Notes for 441](https://devforum.roblox.com/t/release-notes-for-441/686773) that allows you to pack a simple character state in 16 bytes like this:
```
local characterStateFormat = "fffbbbB"

local characterState = string.pack(characterStateFormat,
    posx, posy, posz, dirx * 127, diry * 127, dirz * 127, health)
```
And unpack it like this after network transmission:
```
local posx, posy, posz, dirx, diry, dirz, health =
    string.unpack(characterStateFormat, characterState)
dirx /= 127
diry /= 127
dirz /= 127
```

## Assorted fixes

As usual we fixed a few small problems discovered through testing. We now have an automated process that generates random Luau code in semi-intelligent ways to try to break different parts of our system, and a few fixes this time are a direct result of that.

 * Fix line debug information for multi-line function calls to make sure errors for code like `foo.Bar(...)` are generated in the appropriate location when foo is nil
 * Fix debug information for constant upvalues; this fixes some bugs with watching local variables from the nested functions during debugging
 * Fix an off-by-one range check in string.find for init argument that could result in reading uninitialized memory
 * Fix type confusion for table.move target table argument that could result in reading or writing arbitrary memory
 * Fix type confusion for `debug.getinfo` in some circumstances (we don’t currently expose getinfo but have plans to do so in the future)
 * Improve out of memory behavior for large string allocations in string.rep and some other functions like `table.concat` to handle these conditions more gracefully
 * Fix a regression with `os.time` from last update, where it erroneously reverted to Lua 5.x behavior of treating the time as a local time. Luau version (intentionally) deviates from this by treating the input table as UTC, which matches `os.time()` behavior with no arguments.

## Performance improvements

Only two changes in this category here this time around; some larger scale performance / memory improvements are still pending implementation.

 * Constant locals are now completely eliminated in cases when debugging is not available (so on server/client), making some scripts ~1-2% faster
 * Make script compilation ~5% faster by tuning the compiler analysis and code generation more carefully
Oh, also `math.round` is now a thing which didn’t fit into any category above.