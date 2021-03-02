---
layout: single
title:  "Luau Recap: June 2020"
---

… otherwise known as “This Month in Luau” I guess? You know the drill by now. We’ll talk about exciting things that happened to Luau - our new language stack.

anxiously glances at FIB3 thread that casts a huge shadow on this announcement, but hopefully somebody will read this

Many people work on these improvements; thanks @Apakovtac, @EthicalRobot, @fun_enthusiast, @zeuxcg!

[Originally posted on the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-june-2020/).]

## We have a website!

Many developers told us on many occasions that as much as they love the recaps, it’s hard to know what the state of the language or libraries is if the only way to find out is to read through all updates. What’s the syntax extensions that Luau supports now? How do I use type checking? What’s the status of <my favorite feature> from Lua 5.x?

Well, you can find all of this out here now: [https://roblox.github.io/luau/](https://roblox.github.io/luau/)

Please let us know if this documentation can be improved - what are you missing, what could be improved. For now to maximize change velocity this documentation is separate from DevHub; it’s also meant as an external resource for people who don’t really use the language but are curious about the differences and novelties.

Also, `_VERSION` now returns "Luau" because we definitely aren’t using Lua 5.1 anymore.

## Compound assignments

A long-standing feature request for Lua is compound assignments. Somehow Lua never got this feature, but Luau now implements `+=`, `-=`, `*=`, `/=`, `%=`, `^=` and `..=` operators. We decided to implement them because they are absolutely ubiquitous among most frequently used programming languages, both those with C descent and those with different lineage (Ruby, Python). They result in code that’s easier to read and harder to make mistakes in.

We do not implement `++` and `--`. These aren’t universally adopted, `--` conflicts with comment syntax and they are arguably not as intuitively obvious. We trust everyone to type a few extra characters for `+= 1` without too much trouble.

Two important semantical notes are that the expressions on the left hand side are only evaluated once, so for example `table[makeIndex()] += 1` only runs `makeIndex` once, and that compound assignments still call all the usual metamethod (`__add` et al, and `__index`/`__newindex`) when necessary - you don’t need to change any data structures to work with these.

There’s no noticeable performance improvement from these operators (nor does using them carry a cost) - use them when they make sense for readability.

## Nicer error messages

Good errors are critical to be able to use Luau easily. We’ve spent some time to improve the quality of error messages during parsing and runtime execution:

 * In runtime type errors, we now often use the “Roblox” type name instead of plain userdata, e.g. `math.abs(v)` now says `number` expected, got `Vector3`
 * When arguments are just missing, we now explicitly say that they are missing in libraries like math/table; the old message was slightly more confusing
 * `string.format` in some cases produced error messages that confused missing arguments for incorrect types, which has been fixed
 * When a builtin function such as `math.abs` fails, we now add the function name to the error message. This is something that used to happen in Lua, then we lost this in Luau because Luau removes a very fragile mechanism that supported that, but we now have a new, robust way to report this so you can have the function name back! The message looks like this now: `invalid argument #1 to 'abs' (number expected, got nil)`
 * In compile-time type errors, we now can identify the case when the field was mistyped with a wrong case (ha), and tell you to use the correct case instead.
 * When you forget an `end` statement, we now try to be more helpful and point you to the problematic statement instead of telling you that the end is missing at the very end of the program. This one is using indentation as a heuristic so it doesn’t always work perfectly.
 * We now have slightly more helpful messages for cases when you forget parentheses after a function call
 * We now have slightly more helpful messages for some cases when you accidentally use `( ... )` instead of `{ ... }` to create a table literal
Additionally two places had very lax error checking that made the code more fragile, and we fixed those:

 * `xpcall` now fails immediately when the error function argument is not a function; it used to work up until you get an error, and failed at that point, which made it hard to find these bugs
 * `tostring` now enforces the return type of the result to be a string - previously `__tostring` could return a non-string result, which worked fine up until you tried to do something like passing the resulting value to `string.format` for `%s`. Now `tostring` will fail early.
Our next focus here is better error messages during type checking - please let us know if there are other errors you find confusing and we could improve!

## Type checker improvements

We’re getting closer and closer to be able to move out of beta. A big focus this month was on fixing all critical bugs in the type checker - it now should never hang or crash Studio during type checking, which took a bit of work to iron out all the problems.

Notably, typing function string.length no longer crashes Studio (although why you’d do that is unclear), and Very Large Scripts With Tons Of Nested Statements And Expressions should be stable as well.

We’ve also cleaned up the type information for builtin libraries to make it even more precise, including a few small fixes to `string/math` functions, and a much more precise coroutine library type information. For the latter we’ve introduced a primitive type `thread`, which is what `coroutine` library works with.

## Linter improvements

Linter is the component that produces warnings about scripts; it’s otherwise known as “Static Analysis” in Studio, although that is now serving as a place where we show type errors as well.

Most of the changes here this month are internal as they concern warnings that aren’t yet enabled in Studio (the web site linked above documents all warnings including ones that aren’t active yet but may become active), but once notable feature is that you can now opt out of individual warnings on a script-by-script basis by adding a --!nolint comment to the top of the script. For example, if you really REALLY *REALLY* like the `Game` global, you can add this to the top of the script:

```
--!nolint DeprecatedGlobal
```
Or, if you basically just want us to not issue any warnings ever, I guess you can add this:
```
--!nocheck
--!nolint
```
and live happily ignorant of all possible errors up until you run your code. (please don’t do that)

## os. enhancements

Our overall goal is to try to be reasonably compatible with Lua 5.x in terms of library functions we expose. This doesn’t always work - in some cases we have to remove library features for sandboxing reasons, and in others the library functions don’t make sense in context of Roblox. However, some of these decisions can be revised later. In particular, when we re-added `os.` library to Roblox, we limited it to `os.date`, `os.time` and `os.difftime` (although why `difftime` is a thing isn’t clear), omitting `os.clock` and restricting inputs to `os.date` to return a table with date components, whereas Lua 5.x supports format strings.

Well, this changes today. `os.clock` is now available if you need a high-precision time for benchmarking, and `os.date` can now return formatted date using Lua 5.x format string that you can read about here [https://www.lua.org/pil/22.1.html](https://www.lua.org/pil/22.1.html) (we support all these specifiers: aAbBcdHIjmMpSUwWxXyYzZ).

While `os.date()` is hopefully welcome, `os.clock` may raise some eyebrows - aren’t there enough timing functions in Roblox already? Well, this is nice if you are trying to port code from Lua 5.x to Luau, and there’s this

![Oblig. xkcd]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-june-2020-xkcd.png)

But really, most existing Roblox timing functions are… problematic.

 * `time()` returns the total amount of time the game has been running simulation for, it’s monotonic and has reasonable precision. It’s fine - you can use it to update internal gameplay systems without too much trouble. It should’ve been called “tick” perhaps but that ship has sailed.
 * `elapsedTime` and its close cousin `ElapsedTime`, are telling you “how much time has elapsed since the current instance of Roblox was started.”. While technically true, this isn’t actually useful because on mobile the “start” time here can be days in the past. It’s also inadequate for performance measurements as on Windows, it has a 1ms resolution which isn’t really enough for anything interesting. We’re going to deprecate this in the future.
 * `tick()` sounds perfect - it has a high resolution (usually around 1 microsecond), and a well-defined baseline - it counts since UNIX epoch! Or, well, it actually doesn’t. On Windows, it returns you a variant of the UNIX timestamp in local time zone. In addition, it can be off by 1 second from the actual, real UNIX timestamp, and might have other idiosyncrasies on non-Windows platforms. We’re going to deprecate this in the future

So, if you need a UNIX timestamp, you should use `os.time()`. You get a stable baseline (from 1970’s) and 1s resolution. If you need to measure performance, you should use `os.clock()`. You don’t get a stable baseline, but you get ~1us resolution. If you need to do anything else, you should probably use `time()`.

## Performance optimizations

As you can never have too much performance, we’re continuing to work on performance! We’re starting to look into making Vector3 faster and improving the garbage collector, with some small changes already shipping, but overall it’s a long way out so here are the things that did get visibly better:

 * A few `string.` methods, notably `string.byte` and `string.char`, were optimized to make it easier to write performant deserialization code. string.byte is now ~4x faster than before for small numbers of returned characters. For optimization to be effective, it’s important to call the function directly ( `string.byte(foo, 5)` ) instead of using method calls ( `foo:byte(5)` )
 * Optimize coroutine resumption, making some code that is heavily reliant on `coroutine`. library ~10% faster. We have plans to improve this further, watch this space.
 * Optimize `typeof()` to run ~6x faster. It used to be that `type()` was much faster than `typeof()` but they now should be more or less comparable.
 * Some secret internal optimizations make some scripts a few percent faster
 * The memory allocator used in Luau was rewritten using a new, more efficient, implementation. There might be more changes here in the future to save some memory, but for now this makes some allocation-intensive benchmarks ~15% faster.
 * Using tables with keys that are not strings or numbers is a fair bit more efficient now (most commonly comes up when Instance is used as a key in a hash table), on par with using strings.

Also we found a bug with some of our optimizations (which delayed the string. performance improvement above, but also could affect some math. calls) where in some complex functions you would see valid calls to math. etc. breaking with non-sensical errors such as “expected number, got table” - this has been fixed!

## Memory optimizations

As with performance, our goal here is simple - the more efficient internal Luau structures can become, the less memory will Lua heap take. This is great for both memory consumption, and for garbage collection performance as the collector needs to traverse less data. There’s a few exciting changes in this area this month:

 * Non-array-like tables now take 20% less space. This doesn’t affect arrays but can be observed on object-like tables, both big and small. This is great because some of you are using a lot of large tables apparently, since this resulted in very visible reduction in overall Lua heap sizes across all games.
 * Function objects now take up to 30% less space. This isn’t as impactful since typically function objects are not created very frequently and/or don’t live for very long, but it’s nice nonetheless.
 * New allocator mentioned in the previous section can save up to 5-6% of Lua heap memory as well, although these gains are highly dependent on the workload, and we usually see savings in the 1-2% range.

And that’s it! Till next time. As usual let us know if you have questions, suggestions or bug reports.
