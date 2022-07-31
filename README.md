Luau ![CI](https://github.com/Roblox/luau/workflows/build/badge.svg) [![Coverage](https://coveralls.io/repos/github/Roblox/luau/badge.svg?branch=master&t=2PXMow)](https://coveralls.io/github/Roblox/luau?branch=master)
====

Luau (lowercase u, /ˈlu.aʊ/) is a fast, small, safe, gradually typed embeddable scripting language derived from [Lua](https://lua.org).

It is designed to be backwards compatible with Lua 5.1, as well as incorporating [some features](https://luau-lang.org/compatibility) from future Lua releases, but also expands the feature set (most notably with type annotations). Luau is largely implemented from scratch, with the language runtime being a very heavily modified version of Lua 5.1 runtime, with completely rewritten interpreter and other [performance innovations](https://luau-lang.org/performance). The runtime mostly preserves Lua 5.1 API, so existing bindings should be more or less compatible with a few caveats.

Luau is used by Roblox game developers to write game code, as well as by Roblox engineers to implement large parts of the user-facing application code as well as portions of the editor (Roblox Studio) as plugins. Roblox chose to open-source Luau to foster collaboration within the Roblox community as well as to allow other companies and communities to benefit from the ongoing language and runtime innovation.

This repository hosts source code for the language implementation and associated tooling, documentation for the language as well as RFCs and other materials. The documentation portion of this repository can be viewed at https://luau-lang.org/

# Usage

Luau is an embeddable language, but it also comes with two command-line tools by default, `luau` and `luau-analyze`.

`luau` is a command-line REPL and can also run input files. Note that REPL runs in a sandboxed environment and as such doesn't have access to the underlying file system except for ability to `require` modules.

`luau-analyze` is a command-line type checker and linter; given a set of input files, it produces errors/warnings according to the file configuration, which can be customized by using `--!` comments in the files or [`.luaurc`](https://github.com/Roblox/luau/blob/master/rfcs/config-luaurc.md) files. For details please refer to [type checking]( https://luau-lang.org/typecheck) and [linting](https://luau-lang.org/lint) documentation.

You can download the binaries from [a recent release](https://github.com/Roblox/luau/releases).


# Building

To build Luau tools or tests yourself, you can use CMake on all platforms:

```sh
mkdir cmake && cd cmake
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build . --target Luau.Repl.CLI --config RelWithDebInfo
cmake --build . --target Luau.Analyze.CLI --config RelWithDebInfo
```

Alternatively, on Linux/macOS you can use make:

```sh
make config=release luau luau-analyze
```

To integrate Luau into your CMake application projects, at the minimum you'll need to depend on `Luau.Compiler` and `Luau.VM` projects. From there you need to create a new Luau state (using Lua 5.x API such as `lua_newstate`), compile source to bytecode and load it into the VM like this:

```cpp
// needs lua.h and luacode.h
size_t bytecodeSize = 0;
char* bytecode = luau_compile(source, strlen(source), NULL, &bytecodeSize);
int result = luau_load(L, chunkname, bytecode, bytecodeSize, 0);
free(bytecode);

if (result == 0)
    return 1; /* return chunk main function */
```

For more details about the use of host API you currently need to consult [Lua 5.x API](https://www.lua.org/manual/5.1/manual.html#3). Luau closely tracks that API but has a few deviations, such as the need to compile source separately (which is important to be able to deploy VM without a compiler), or lack of `__gc` support (use `lua_newuserdatadtor` instead).

To gain advantage of many performance improvements it's highly recommended to use `safeenv` feature, which sandboxes individual scripts' global tables from each other as well as protects builtin libraries from monkey-patching. For this to work you need to call `luaL_sandbox` for the global state and `luaL_sandboxthread` for each new script's execution thread.

# Testing

Luau has an internal test suite; in CMake builds it is split into two targets, `Luau.UnitTest` (for bytecode compiler and type checker/linter tests) and `Luau.Conformance` (for VM tests). The unit tests are written in C++, whereas the conformance tests are largely written in Luau (see `tests/conformance`).

Makefile builds combine both into a single target and can be ran via `make test`.

# Dependencies

Luau uses C++ as its implementation language. The runtime requires C++11, whereas the compiler and analysis components require C++17. It should build without issues using Microsoft Visual Studio 2017 or later, or gcc-7 or clang-7 or later.

Other than the STL/CRT, Luau library components don't have external dependencies. The test suite depends on [doctest](https://github.com/onqtam/doctest) testing framework, and the REPL command-line depends on [isocline](https://github.com/daanx/isocline).

# License

Luau implementation is distributed under the terms of [MIT License](https://github.com/Roblox/luau/blob/master/LICENSE.txt). It is based on Lua 5.x implementation that is MIT licensed as well.

When Luau is integrated into external projects, we ask to honor the license agreement and include Luau attribution into the user-facing product documentation. The attribution using [Luau logo](https://github.com/Roblox/luau/blob/master/docs/logo.svg) is also encouraged.
