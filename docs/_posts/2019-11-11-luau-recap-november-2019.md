---
layout: single
title:  "Luau Recap: November 2019"
---

A few months ago, we’ve released our new Lua implementation, Luau ([Faster Lua VM Released](https://devforum.roblox.com/t/faster-lua-vm-released/339587)) and made it the default for most platforms and configurations. Since then we’ve shipped many smaller changes that improved performance and expanded the usability of the VM. Many of them have been noted in release notes but some haven’t, so here’s a recap of everything that has happened in the Lua land since September!

[Originally posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-november-2019/).]

## Debugger beta

When we launched the new VM, we did it without the full debugger support. The reason for this is that the new VM is substantially different and the old implementation of the debugger (that relied on line hooks) just doesn’t work.

We had to rebuild the low level implementation of the debugger from scratch - this is a tricky problem and it took time! We are excited to share a beta preview of this with you today.

To use this, simply make sure that you’re enrolled in the new Lua VM beta:

![Enable New Lua VM]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-november-2019-option.png)

After this you can use the debugger as usual. If you see any bugs, please feel free to report them!

## Performance improvements

 * The for loop optimization that specializes `pairs/ipairs` now works for localized versions of these globals as well, as well as `next, table` expressions
 * a^k expressions are now faster for some trivial values of k such as 2 and 0.5
 * Calling methods and accessing properties on deeply nested Roblox objects is now significantly faster than it used to be (~2x faster for objects that have an 8-deep nesting) - the cost is now independent of the hierarchy depth.
 * Accessing .X/.Y/.Z properties on Vector types is now ~30% faster
 * On Windows and Xbox, we’ve tuned our interpreter to be ~5-6% faster on Lua-intensive code
 * For a set of builtin functions, we now support very quickly calling them from VM via a new fastcall mechanism.

Fastcall requires the function call to be present in source as a global or localized global access (e.g. either `math.max(x, 1)` or `max(x, 1) where local max = math.max`). This can be substantially faster than normal calls, e.g. this makes SHA256 benchmark ~1.7x faster. We are currently optimizing calls to `bit32`, `math` libraries and additionally `assert` and `type`. Also, just like other global-based optimizations, this one is disabled if you use `getfenv`/`setfenv`.

## Lua library extensions

We’ve implemented most library features available in later versions of upstream Lua, including:

 * `table.pack` and `table.unpack` from Lua 5.2 (the latter is same as global `unpack`, the former helps by storing the true argument count in `.n` field)
 * `table.move` from Lua 5.3 (useful for copying data between arrays)
 * `coroutine.isyieldable` from Lua 5.3
 * `math.log` now accepts a second optional argument (as seen in Lua 5.2) for the logarithm base

We’ve also introduced two new functions in the table library:

 * `table.create(count, value)` can create an array-like table quickly
 * `table.find(table, value [, init])` can quickly find the numeric index of the element in the table
 
Autocomplete support for `table.create`/`table.find` will ship next week

## Lua syntax extensions

We’ve started taking a look at improving the Lua syntax. To that end, we’ve incorporated a few changes from later versions of Lua into the literal syntax:

 * String literals now support `\z` (skip whitespace), `\x` (hexadecimal byte) and `\u` (Unicode codepoint) escape sequences

and implemented a few extra changes:

 * Number literals now support binary literals, e.g. `0b010101`
 * Number literals now support underscores anywhere in the literal for easier digit grouping, e.g. `1_000_000`

Note that the literal extensions aren’t currently supported in syntax highlighter in Studio but this will be amended soon.

## Error messages

Error messages are slowly getting a bit of love. We’ve improved some runtime errors to be nicer, in particular:

 * When indexing operation fails, we now specify the key name or type, e.g. “attempt to index foo with ‘Health’”
 * When arithmetic operations fails, we now specify the type of arithmetic operation, e.g. “attempt to perform arithmetic (add) on table and number”

We’ve also improved some parse errors to look nicer by providing extra context - for example, if you forget parentheses after function name in a function declaration, we will now say `Expected '(' when parsing function, got 'local'`.

We are looking into some reports of misplaced line numbers on errors in multi-line expressions but this will only ship later.

## Correctness fixes

There are always a few corner cases that we miss - a new Lua implementation is by necessity subtly different in a few places. Our goal is to find and correct as many of these issues as possible. In particular, we’ve:

 * Fixed some cases where we wouldn’t preserve negative zero (`-0`)
 * Fixed cases where `getfenv(0)` wouldn’t properly deoptimize access to builtin globals
 * Fixed cases where calling a function with >255 parameters would overflow the stack
 * Fixed errors with very very very long scripts and control flow around large blocks (thousands of lines of code in a single if/for statement)
 * Fixed cases where in Studio on Windows, constant-time comparisons with `NaNs` didn’t behave properly (`0/0==1`)

Also, the upvalue limit in the new VM has been raised to 200 from 60; the limit in Lua 5.2 is 255 but we decided for now to match the local limit.

## Script analysis

Along with the compiler and virtual machine, we’ve implemented a new linting framework on top of Luau which is similar to our old script analysis code but is richer. In particular, we support a few more checks that are enabled by default:

 * Unreachable code warning, for cases where function provably doesn’t reach a specific point, such as redundant return after a set of if/else statements where every branch returns or errors.
 * Unknown type warning, which was emitted before for `Instance.new/GetService/IsA` calls, is now also emitted when the result of `type/typeof` is compared to a string literal
 * We now recognize and flag mistaken attempts to iterate downwards with a for loop (such as `for i=9,1` or `for i=#t,1` as well as cases where numeric for loop doesn’t reach the stated target (`for i=1,4.5`)
 * We now detect and flag cases where in assignment expressions variables are implicitly initialized with nil or values are dropped during assignment
 * “Statement spans multiple lines” warning now does not trigger on idiomatic constructs involving setting up locals in a block (`local name do ... name = value ... end`)

We also have implemented a few more warnings for common style/correctness issues but they aren’t enabled yet - we’re looking into ways for us to enable them without too much user impact:

 * Local variables that shadow other local variables / global variables
 * Local variables that are assigned but never used
 * Implicit returns, where functions that explicitly return values in some codepaths can reach the end of the function and implicitly return no values (which is error-prone)

## Future plans

There’s a fair bit of performance improvements that we haven’t gotten to yet that are on the roadmap - this includes general VM optimizations (faster function calls, faster conditional checks, faster error handling including `pcall`) and some library optimizations (in particular, Vector3 math performance improvements). And we’re starting to look into some exciting ways for us to make performance even better in the future.

Also we’re still working on the type system! It’s starting to take shape and we should have something ready for you by the end of the year, but you’ll learn about it in a separate post :smiley:

As always don’t hesitate to reach out if you have any issues or have any suggestions for improvements.

