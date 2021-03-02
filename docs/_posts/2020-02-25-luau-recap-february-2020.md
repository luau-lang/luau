---
layout: single
title:  "Luau Recap: February 2020"
---

We continue to iterate on our language stack, working on many features for type checking, performance, and quality of life. Some of them come with announcements, some come with release notes, and some just ship - here we will talk about all things that happened since November last year.

A lot of people work on these improvements; thanks @Apakovtac, @EthicalRobot, @fun_enthusiast, @xyzzyismagic, @zeuxcg!

[Originally posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-february-2020/).]

We were originally intending to ship the beta last year but had to delay it due to last minute bugs. However, it’s now live as a beta option on production! Go here to learn more:

EDIT: Please DO NOT publish places with type annotations just yet as they will not work on production! This is why it’s a beta 🙂 However, please continue to experiment in Studio and give us feedback. We are reading everything and will be fixing reported bugs and discussing syntax / semantics issues some people brought up. Hello! We’ve been quietly working on building a type checker for Lua for quite some time now. It is now far enough along that we’d really like to hear what…

We’re continuing to iterate on the feedback we have received here. Something that will happen next is that we will enable type annotations on live server/clients - which will mean that you will be able to publish source code with type annotations without breaking your games. We still have work to do on the non-strict and strict mode type checking before the feature can move out of beta though, in particular we’ve implemented support for require statement and that should ship next week 🤞

We also fixed a few bugs in the type definitions for built-in functions/API and the type checker itself:

 * `table.concat` was accidentally treating the arguments as required
 * `string.byte` and `string.find` now have a correct precise type
 * `typeof` comparisons in if condition incorrectly propagated the inferred type into `elseif` branches

We are also making the type checker more ergonomic and more correct. Two changes I want to call out are:

 * Type aliases declared with `type X = Y` are now co-recursive, meaning that they can refer to each other, e.g.

```
type array<T> = { [number]: T }

type Wheel = { radius: number, car: Car }
type Car = { wheels: array<Wheel> }
```

* We now support type intersections `(A & B)` in addition to type unions `(A | B)`. Intersections are critical to modeling overloaded functions correctly - while Lua as a language doesn’t support function overloads, we have various APIs that have complex overloaded semantics - one of them that people who tried the beta had problems with was UDim2.new - and it turns out that to correctly specify the type, we had to add support for intersections. This isn’t really intended as a feature that is used often in scripts developers write, but it’s important for internal use.

## Debugger (beta)

When we shipped the original version of the VM last year, we didn’t have the debugger fully working. Debugger relies on low-level implementation of the old VM that we decided to remove from the new VM - as such we had to make a new low-level debugging engine.

This is now live under the Luau VM beta feature, see [this post](https://devforum.roblox.com/t/luau-in-studio-beta/456529) for details.

If you use the debugger at all, please enable the beta feature and try it out - we want to fix all the bugs we can find, and this is blocking us enabling the new VM everywhere.

(a quick aside: today the new VM is enabled on all servers and all clients, and it’s enabled in Studio “edit” mode for plugins - but not in Studio play modes, and full debugger support is what prevents us from doing so)

## Language

This section is short and sweet this time around:

* You can now use continue statement in for/while/repeat loops. :tada:

Please note that we only support this in the new VM, so you have to be enrolled in Luau VM beta to be able to use it in Studio. It will work in game regardless of the beta setting as per above.

## Performance

While we have some really forward looking ideas around multi-threading and native code compilation that we’re starting to explore, we also continue to improve performance across the board based on our existing performance backlog and your feedback.

In particular, there are several memory and performance optimizations that shipped in the last few months:

 * Checking for truth (`if foo or foo and bar`) is now a bit faster, giving 2-3% performance improvements on some benchmarks
 * `table.create` (with value argument) and `table.pack` have been reimplemented and are ~1.5x faster than before
 * Internal mechanism for filling arrays has been made faster as well, which makes `Terrain:ReadVoxels` ~10% faster
 * Catching engine-generated errors with pcall/xpcall is now ~1.5x faster (this only affects performance of calls that generated errors)
 * String objects now take 8 bytes less memory per object (and in an upcoming change we’ll save a further 4 bytes)
 * Capturing local variables that are never assigned to in closures is now much faster, takes much less memory and generates much less GC pressure. This can make closure creation up to 2x faster, and improves some Roact benchmarks by 10%. This is live in Studio and will ship everywhere else shortly.
 * The performance of various for loops (numeric & ipairs) on Windows regressed after a VS2017 upgrade; this regression has been fixed, making all types of loops perform roughly equally. VS2017 upgrade also improved Luau performance on Windows by ~10% across the board.
 * Lua function calls have been optimized a bit more, gaining an extra 10% of performance in call-heavy benchmarks on Windows.
 * Variadic table constructors weren’t compiled very efficiently, resulting in surprisingly low performance of constructs like `{...}`. Fixing that made `{...}` ~3x faster for a typical number of variadic arguments.

## Diagnostics

We spent some time to improve error messages in various layers of the stack based on the reports from community. Specifically:

 * The static analysis warning about incorrect bounds for numeric for loops is now putting squigglies in the right place.
 * Fixed false positive static analysis warnings about unreachable code inside repeat…until loops in certain cases.
 * Multiline table construction expressions have a more precise line information now which helps in debugging since callstacks are now easier to understand
 * Incomplete statements (e.g. foo) now produce a more easily understandable parsing error
 * In some cases when calling the method with a `.` instead of `:`, we emitted a confusing error message at runtime (e.g. humanoid.LoadAnimation(animation)). We now properly emit the error message asking the user if `:` was intended.
 * The legacy global `ypcall` is now flagged as deprecated by script analysis
 * If you use a Unicode symbol in your source program outside of comments or string literals, we now produce a much more clear message, for example:
```
local pi = 3․13 -- spoiler alert: this is not a dot!
```
produces `Unexpected Unicode character: U+2024. Did you mean '.'?`

## LoadLibrary removal

Last but not least, let’s all press [F for LoadLibrary](https://devforum.roblox.com/t/loadlibrary-is-going-to-be-removed-on-february-3rd/382516).

It was fun while it lasted, but supporting it caused us a lot of pain over the years and prevented some forward-looking changes to the VM. We don’t like removing APIs from the platform, but in this case it was necessary. Thanks to the passionate feedback from the community we adjusted our initial rollout plans to be less aggressive and batch-processed a lot of gear items that used this function to stop using this function. The update is in effect and LoadLibrary is no more.

As usual, if you have any feedback about any of these updates, suggestions, bug reports, etc., post them in this thread or (preferably for bugs) as separate posts in the bug report category.
