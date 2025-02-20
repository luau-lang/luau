Thanks for deciding to contribute to Luau! These guidelines will try to help make the process painless and efficient.

## Questions

If you have a question regarding the language usage/implementation, please [use GitHub discussions](https://github.com/luau-lang/luau/discussions).
Some questions just need answers, but it's nice to keep them for future reference in case other people want to know the same thing.
Some questions help improve the language, implementation or documentation by inspiring future changes.

## Documentation

A [separate site repository](https://github.com/luau-lang/site) hosts the language documentation, which is accessible on https://luau.org.
Changes to this documentation that improve clarity, fix grammatical issues, explain aspects that haven't been explained before and the like are warmly welcomed.

Please feel free to [create a pull request](https://help.github.com/articles/about-pull-requests/) to improve our documentation. Note that at this point the documentation is English-only.

## Bugs

If the language implementation doesn't compile on your system, compiles with warnings, doesn't seem to run correctly for your code or if anything else is amiss, please [open a GitHub issue](https://github.com/luau-lang/luau/issues/new).
It helps if you note the Git revision issue happens in, the version of your compiler for compilation issues, and a reproduction case for runtime bugs.

Of course, feel free to [create a pull request](https://help.github.com/articles/about-pull-requests/) to fix the bug yourself.

## Features

If you're thinking of adding a new feature to the language, library, analysis tools, etc., please *don't* start by submitting a pull request.
The Luau team has internal priorities and a roadmap that may or may not align with specific features, so before starting to work on a feature, please submit an issue describing the missing feature that you'd like to add.

For features that result in an observable change to the language's syntax or semantics, you'll need to [create an RFC](https://github.com/luau-lang/rfcs/blob/master/README.md) to make sure that the feature is needed and well-designed.

Finally, please note that Luau tries to carry a minimal feature set. All features must be evaluated not just for the benefits that they provide, but also for the downsides/costs in terms of language simplicity, maintainability, cross-feature interaction, etc.
As such, feature requests may not be accepted even if a comprehensive RFC is written - don't expect Luau to gain a feature just because another programming language has it.
We generally apply a standard similar to the C\# team's famous [Minus 100 Points](https://learn.microsoft.com/en-us/archive/blogs/ericgu/minus-100-points).

## Code style

Contributions to this project are expected to follow the existing code style.
`.clang-format` file mostly defines syntactic styling rules (you can run `make format` to format the code accordingly).

As for naming conventions, most Luau components use `lowerCamelCase` for variables and functions, `UpperCamelCase` for types and enums, `kCamelCase` for global constants and `SCARY_CASE` for macros.

Within the VM component, the code style is different - we expect `lua_` or `luaX_` prefix for functions that are public or used across different VM files, camel case isn't used and macros are often using lowercase.

## Testing

All pull requests will run through a continuous integration pipeline using GitHub Actions that will run the built-in unit tests and integration tests on Windows, macOS and Linux.
You can run the tests yourself using `make test` or using `cmake` to build `Luau.UnitTest` and `Luau.Conformance` and run them.

When making code changes please try to make sure they are covered by an existing test or add a new test accordingly.

## Performance

One of the central features of Luau is performance; our runtime in particular is heavily optimized for high performance and low memory consumption, and code is generally carefully tuned to result in close-to-optimal assembly for x64 and AArch64 architectures. The analysis code is not optimized to the same level of detail, but performance is still very important to make sure that we can support interactive IDE features.

As such, it's important to make sure that the changes, including bug fixes, improve (or at least do not regress) performance. For the VM, this can be validated by running `bench/bench.py` on two binaries built in Release mode, before and after the changes. Note that our benchmark coverage is not complete, and in some cases, additional performance testing will be necessary to determine if the change can be merged.

## Feature flags

For large bug fixes or features that apply to the Luau components and not just the CLI tools, we may ask that you introduce a feature flag to gate your changes. The feature flags use `LUAU_FASTFLAG` macro family defined in `Luau/Common.h` and allow us to ensure that the change can be safely shipped, enabled, and rolled back on the Roblox platform when the change makes it into our production codebase. The tests run the code with flags in their default state and enabled state as well to ensure correctness.

## Licensing

By contributing changes to this repository, you license your contribution under the MIT License, and you agree that you have the right to license your contribution under those terms.
