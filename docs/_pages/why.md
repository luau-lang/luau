---
permalink: /why
title: Why Luau?
---

Around 2006, [Roblox](https://www.roblox.com) started using Lua 5.1 as a scripting language for games. Over the years the runtime had to be tweaked to provide a safe, secure sandboxed environment; we gradually started accumulating small library changes and tweaks.

Over the course of the last few years, instead of using Web-based stack for our player-facing application, Lua-based in-game UI and Qt-based editor UI, we've started consolidating a lot of the efforts and developing all of these using Roblox engine and Lua as a scripting language.

Having grown a substantial internal codebase that needed to be correct and performant, and with the focus shifting a bit from novice game developers to professional studios building games on Roblox and our own teams of engineers building applications, there was a need to improve performance and quality of the code we were writing.

Unlike mainline Lua, we also could not afford to do major breaking changes to the language (hence the 5.1 language baseline that remained unchanged for more than a decade). While faster implementations of Lua 5.1 like LuaJIT were available, they didn't meet our needs in terms of portability, ease of change and they didn't address the problem of developing robust code at scale.

All of these motivated us to start reshaping Lua 5.1 that we started from into a new, derivative language that we call Luau. Our focus is on making the language more performant and feature-rich, and make it easier to write robust code through a combination of linting and type checking using a gradual type system.

## Complete rewrite?

A very large part of Luau codebase is written from scratch. We needed a set of tools to be able to write language analysis tools; Lua has a parser that is integrated with the bytecode compiler, which makes it unsuitable for complex semantic analysis. For bytecode compilation, while a single pass compiler can deliver better compilation throughput and be simpler than a full frontend/backend, it significantly limits the optimizations that can be done at the bytecode level.

Luau compiler and analysis tools are thus written from scratch, closely following the syntax and semantics of Lua. Our compiler is not single-pass, and instead relies on a set of analysis passes that run over the AST to produce efficient bytecode, followed by some post-process optimizations.

As for the runtime, we had to rewrite the interpreter from scratch to get substantially faster performance; using a combination of techniques pioneered by LuaJIT and custom optimizations that are able to improve performance by taking control over the entire stack (language, compiler, interpreter, virtual machine), we're able to get close to LuaJIT interpreter performance while using C as an implementation language.

The garbage collector and the core libraries represent more of an incremental change, where we used Lua 5.1 as a baseline but we're continuing to rewrite these as well with performance in mind.

While Luau doesn't currently implement JIT/AOT, this is likely to happen at some point; beyond the usual implementation challenges and security concerns, one significant limitation is that we don't have access to JIT on many platforms so for us maintaining excellent interpreted performance for gameplay and application code is more important than reaching peak FLOPS on numerical code.
