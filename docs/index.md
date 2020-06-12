Luau
====

Luau is a fast, small, safe, gradually typed embeddable scripting language derived from Lua. It is used by Roblox game developers to write game code, as well as by Roblox engineers to implement large parts of the user-facing application code as well as portions of the editor (Roblox Studio) as plugins.

```lua
type Point = { x: number, y: number }

local p = { x = 1, y = 2 }

print(p.x, p.y)
-- print(p.z) results in a type error
```

Motivation
==========

Around 2006, [Roblox](https://www.roblox.com) started using Lua 5.1 as a scripting language for games. Over the years we ended up substantially evolving the implementation and the language; to support growing sophistication of games on the Roblox platform, growing team sizes and large internal teams writing a lot of code for application/editor (400KLOC as of 2020), we had to invest in performance, ease of use and language tooling, and introduce a gradual type system to the language. [This page](why.md) goes into more detail about the road that got us here.

Syntax
======

Luau is syntactically backwards-compatible with Lua 5.1 (code that is valid Lua 5.1 is also valid Luau); however, we have extended the language with a set of syntactical features that make the language more familiar and ergonomic. The syntax [is described here](syntax.md).

Sandboxing
==========

Luau limits the set of standard libraries exposed to the users and implements extra sandboxing features to be able to run unprivileged code (written by our game developers) side by side with privileged code (written by us). This results in an execution environment that is different from what is commonplace in Lua. The sandboxing [is described here](sandbox.md).


Compatibility
=============

Whenever possible, Luau aims to be backwards-compatible with Lua 5.1 and at the same time to incorporate features from later revisions of Lua. However, Luau is not a full superset of later versions of Lua - we do not agree with some design decisions and are not constrained by the same reasoning as that of PUC-Rio. All post-5.1 Lua features, along with their support status in Luau, [are documented here](compatibility.md).

Analysis
========

To make it easier to write correct code, Luau comes with a set of analysis tools that can surface common mistakes. These consist of a linter and a type checker, colloquially known as "script analysis", and can be used from [Roblox Studio](https://developer.roblox.com/en-us/articles/The-Script-Analysis-Tool) or using SECRET TOOL. The linting passes are [described here](lint.md), and the type checking user guide can [be found here](typecheck.md).

Libraries
=========

As a language, Luau is a full superset of Lua 5.1. As far as standard library is concerned, some functions had to be removed from the builtin libraries, and some functions had to be added. Additionally, Luau is currently only runnable from the context of the Roblox engine, which exposes a large API surface [documented on Roblox developer portal](https://developer.roblox.com/en-us/api-reference).

Performance
===========

In addition to a completely custom front end that implements parsing, linting and type checking, Luau runtime features new bytecode, interpreter and compiler that are heavily tuned for performance. Luau currently does not implement Just-In-Time compilation, but its interpreter is often competitive with LuaJIT interpreter on a wide set of benchmarks. We continue to optimize the runtime and rewrite portions of it to be even more efficient, including plans for a new garbage collector and further library optimizations, as well as an eventual JIT/AOT option. While our overall goal is to minimize the amount of time programmers spend tuning performance, some details about the performance characteristics are [provided for inquisitive minds](performance.md).
