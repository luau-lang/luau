---
permalink: /compatibility
title: Compatibility
toc: true
---

Luau is based on Lua 5.1, and as such incorporates all features of 5.1, except for ones that had to be taken out due to sandboxing limitations. Because of backwards compatibility constraints, we don't remove features deprecated by later versions (e.g. we still support `getfenv`/`setfenv`). Later Lua versions introduce new features into the language and new libraries/functions.

Our overall goal is to incorporate features from the later versions of Lua when it makes sense for us to do so - the motivations behind some newer features are unclear or don't apply to the domain Luau is used in, and many features carry costs that don't always make sense to pay. The rest of this document describes the status of all features of Lua 5.2 and beyond, with the following classification:

- ✔️ - the feature is available in Luau
- ❌ - the feature is not available in Luau because we don't believe it makes sense to include it
- 😞 - the feature is not available in Luau because of compatibility/sandboxing concerns
- 🔜 - the feature is not available in Luau yet but we'd like to include it and are possibly working on it
- 🤷‍♀️ - the feature is not available in Luau yet; we don't have strong opinions on it so it might make it at some point

Please note that all of these decisions are not final, they are just our current stance. In some cases evolution of our VM may make a feature that was previously impractical to support due to performance complications feasible. In some cases a feature that didn't have a strong use case gains one, so we can implement it.

## Implementation limits

Luau has certain limitations around the number of local variables, registers, upvalues, constants and instructions. These limits are often different from the limits imposed by various versions of Lua, and are documented here without promising that future versions will adhere to these. Note that writing code that is close to any of these limits is dangerous because this code may become invalid as our codegen evolves.

- Local variables: 200 per function (same as all versions of Lua, this includes function arguments)
- Upvalues: 200 per function (up from 60 in Lua 5.1)
- Registers: 255 per function (same as all versions of Lua, this includes local variables and function arguments)
- Constants: 2^23 per function (up from 2^18 in Lua 5.1)
- Instructions: 2^23 per function (up from 2^17 in Lua 5.1, although in both cases the limit only applies to control flow)
- Nested functions: 2^15 per function (down from 2^18 in Lua 5.1)
- Stack depth: 20000 Lua calls per Lua thread, 200 C calls per C thread (e.g. `coroutine.resume`/`pcall` nesting is limited to 200)

Note that Lua 5.3 has a larger upvalue limit (255) and a larger constant limit (2^26); existing Luau limits are likely sufficient for reasonable use cases.

## Lua 5.1

Since several features were removed from Lua 5.1 for sandboxing reasons, this table lists them for completeness.

| feature | notes |
|---------|------|
| `io`, `os`, `package` and `debug` library | note that some functions in `os`/`debug` are still present |
| `loadfile`, `dofile` | removed for sandboxing, no direct file access |
| `loadstring` bytecode and `string.dump` | exposing bytecode is dangerous for sandboxing reasons |
| `newproxy` can only be called with nil or boolean | extra flexibility removed for sandboxing |

Sandboxing challenges are [covered in the dedicated section](sandbox).

## Lua 5.2

| feature | status | notes |
|---------|--------|------|
| yieldable pcall/xpcall | ✔️ | |
| yieldable metamethods | ❌ | significant performance implications |
| ephemeron tables | ❌ | this complicates and slows down the garbage collector esp. for large weak tables |
| emergency garbage collector | 🤷‍ | Luau runs in environments where handling memory exhaustion in emergency situations is not tenable |
| goto statement | ❌ | this complicates the compiler, makes control flow unstructured and doesn't address a significant need |
| finalizers for tables | ❌ | no `__gc` support due to sandboxing and performance/complexity |
| no more fenv for threads or functions | 😞 | we love this, but it breaks compatibility |
| tables honor the `__len` metamethod | 🤷‍♀️ | performance implications, no strong use cases
| hex and `\z` escapes in strings | ✔️ | |
| support for hexadecimal floats | 🤷‍♀️ | no strong use cases |
| order metamethods work for different types | ❌ | no strong use cases and more complicated semantics, compatibility and performance implications |
| empty statement | 🤷‍♀️ | less useful in Lua than in JS/C#/C/C++ |
| `break` statement may appear in the middle of a block | 🤷‍♀️ | we'd like to do it consistently for `break`/`return`/`continue` but there be dragons |
| arguments for function called through `xpcall` | ✔️ | |
| optional base in `math.log` | ✔️ | |
| optional separator in `string.rep` | 🤷‍♀️ | no strong use cases |
| new metamethods `__pairs` and `__ipairs` | ❌ | superseded by `__iter` |
| frontier patterns | ✔️ | |
| `%g` in patterns | ✔️ | |
| `\0` in patterns | ✔️ | |
| `bit32` library | ✔️ | |
| `string.gsub` is stricter about using `%` on special characters only | ✔️ | |
| light C functions | 😞 | this changes semantics of fenv on C functions and has complex implications wrt runtime performance |

Two things that are important to call out here are various new metamethods for tables and yielding in metamethods. In both cases, there are performance implications to supporting this - our implementation is *very* highly tuned for performance, so any changes that affect the core fundamentals of how Lua works have a price. To support yielding in metamethods we'd need to make the core of the VM more involved, since almost every single "interesting" opcode would need to learn how to be resumable - which also complicates future JIT/AOT story. Metamethods in general are important for extensibility, but very challenging to deal with in implementation, so we err on the side of not supporting any new metamethods unless a strong need arises.

For `__pairs`/`__ipairs`, we felt that extending library functions to enable custom containers wasn't the right choice. Instead we revisited iteration design to allow for self-iterating objects via `__iter` metamethod, which results in a cleaner iteration design that also makes it easier to iterate over tables. As such, we have no plans to support `__pairs`/`__ipairs` as all use cases for it can now be solved by `__iter`. 

Ephemeron tables may be implemented at some point since they do have valid uses and they make weak tables semantically cleaner, however the cleanup mechanism for these is expensive and complicated, and as such this can only be considered after the pending GC rework is complete.

## Lua 5.3

| feature | status | notes |
|---------|--------|------|
| `\u` escapes in strings | ✔️ | |
| integers (64-bit by default) | ❌ | backwards compatibility and performance implications |
| bitwise operators | ❌ | `bit32` library covers this in absence of 64-bit integers |
| basic utf-8 support | ✔️ | we include `utf8` library and other UTF8 features |
| functions for packing and unpacking values (string.pack/unpack/packsize) | ✔️ | |
| floor division | ❌ | no strong use cases, syntax overlaps with C comments |
| `ipairs` and the `table` library respect metamethods | ❌ | no strong use cases, performance implications |
| new function `table.move` | ✔️ | |
| `collectgarbage("count")` now returns only one result | ✔️ | |
| `coroutine.isyieldable` | ✔️ | |
| stricter error checking for `table.insert`/`table.remove` | 😞 | we love this, but it breaks compatibility

It's important to highlight integer support and bitwise operators. For Luau, it's rare that a full 64-bit integer type is necessary - double-precision types support integers up to 2^53 (in Lua which is used in embedded space, integers may be more appealing in environments without a native 64-bit FPU). However, there's a *lot* of value in having a single number type, both from performance perspective and for consistency. Notably, Lua doesn't handle integer overflow properly, so using integers also carries compatibility implications.

If integers are taken out of the equation, bitwise operators make less sense, as integers aren't a first class feature; additionally, `bit32` library is more fully featured (includes commonly used operations such as rotates and arithmetic shift; bit extraction/replacement is also more readable). Adding operators along with metamethods for all of them increases complexity, which means this feature isn't worth it on the balance. Common arguments for this include a more familiar syntax, which, while true, gets more nuanced as `^` isn't available as a xor operator, and arithmetic right shift isn't expressible without yet another operator, and performance, which in Luau is substantially better than in Lua because `bit32` library uses VM builtins instead of expensive function calls.

Floor division is much less complex, but it's used rarely enough that `math.floor(a/b)` seems like an adequate replacement; additionally, `//` is a comment in C-derived languages and we may decide to adopt it in addition to `--` at some point.

## Lua 5.4

| feature | status | notes |
|--|--|--|
| new generational mode for garbage collection | 🔜 | we're working on gc optimizations and generational mode is on our radar
| to-be-closed variables | ❌ | the syntax is inconsistent with how we'd like to do attributes long-term; no strong use cases in our domain |
| const variables | ❌ | while there's some demand for const variables, we'd never adopt this syntax |
| new implementation for math.random | ✔️ | our RNG is based on PCG, unlike Lua 5.4 which uses Xoroshiro |
| optional `init` argument to `string.gmatch` | 🤷‍♀️ | no strong use cases |
| new functions `lua_resetthread` and `coroutine.close` | ✔️ ||
| coercions string-to-number moved to the string library | 😞 | we love this, but it breaks compatibility |
| new format `%p` in `string.format` | 🤷‍♀️ | no strong use cases |
| `utf8` library accepts codepoints up to 2^31 | 🤷‍♀️ | no strong use cases |
| The use of the `__lt` metamethod to emulate `__le` has been removed | ❌ | breaks compatibility and complicates comparison overloading story |
| When finalizing objects, Lua will call `__gc` metamethods that are not functions | ❌ | no `__gc` support due to sandboxing and performance/complexity |
| The function print calls `__tostring` instead of tostring to format its arguments. | ✔️ | |
| By default, the decoding functions in the utf8 library do not accept surrogates. | 😞 | breaks compatibility and doesn't seem very interesting otherwise |

Taking syntax aside (which doesn't feel idiomatic or beautiful), `<close>` isn't very useful in Luau - its dominant use case is for code that works with external resources like files or sockets, but we don't provide such APIs - and has a very large complexity cost, evidences by a lot of bug fixes since the initial implementation in 5.4 work versions. `<const>` in Luau doesn't matter for performance - our multi-pass compiler is already able to analyze the usage of the variable to know if it's modified or not and extract all performance gains from it - so the only use here is for code readability, where the `<const>` syntax is... suboptimal.

If we do end up introducing const variables, it would be through a `const var = value` syntax, which is backwards compatible through a context-sensitive keyword similar to `type`. That said, there's ambiguity wrt whether `const` should simply behave like a read-only variable, ala JavaScript, or if it should represent a stronger contract, for example by limiting the expressions on the right hand side to ones compiler can evaluate ahead of time, or by freezing table values and thus guaranteeing immutability.

## Differences from Lua

We have a few behavior deviations from Lua 5.x that come from either a different implementation, or our desire to clean up small inconsistencies in the language/libraries:

* Tail calls are not supported to simplify implementation, make debugging/stack traces more predictable and allow deep validation of caller identity for security
* Order of table assignment in table literals follows program order in mixed tables (Lua 5.x assigns array elements first in some cases)
* Equality comparisons call `__eq` metamethod even when objects are rawequal (which matches other metamethods like `<=` and facilitates NaN checking)
* `function()` expressions may reuse a previously created closure in certain scenarios (when all upvalues captured are the same) for efficiency, which changes object identity but doesn't change call semantics -- this is different from Lua 5.1 but similar to Lua 5.2/5.3
* `os.time` returns UTC timestamp when called with a table for consistency
