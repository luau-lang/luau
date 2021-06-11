# Deprecate getfenv/setfenv

## Summary

Mark getfenv/setfenv as deprecated

## Motivation

getfenv and setfenv are problematic for a host of reasons:

- They allow uncontrolled mutation of global environment, which results in deoptimization; various important performance features
like builtin calls or imports are disabled when these functions are used.
- Because of the uncontrolled mutation code that uses getfenv/setfenv can't be typechecked correctly; in particular, injecting new
globals is going to produce "unknown globals" warnings, and modifying existing globals can trivially violate soundness wrt type
checking
- While these functions can be used for good (once you ignore the issues above), such as custom module systems, statistically speaking
they are mostly used to obfuscate code to hide malicious intent.

## Design

We will mark getfenv and setfenv as deprecated. The only consequence of this change is that the linter will start emitting warnings when they are used.

Removing support for getfenv/setfenv, while tempting, is not planned in the foreseeable future because it will cause significant backwards compatibility issues.

## Drawbacks

There are valid uses for getfenv/setfenv, that include extra logging (in Roblox code this manifests as `getfenv(1).script`), monkey patching for mocks in unit tests, and custom
module systems that inject globals into the calling environment. We do have a replacement for logging use cases, `debug.info`, and we do have an officially recommended replacement
for custom module systems, which is to use `require` that doesn't result in issues that fenv modification carries and can be understood by the type checker, we do not have an
alternative for mocks. As such, testing frameworks that implement mocking via setfenv/getfenv will need to use `--!nolint DeprecatedGlobal` to avoid this warning.

## Alternatives

Besides the obvious alternative "do nothing", we could also consider implementing Lua 5.2 support for _ENV. However, since we do not have a way to load script files other than
via `require` that doesn't support _ENV, and `loadstring` is supported but discouraged, we do not currently plan to implement `_ENV` although it's possible that this will happen
in the future.
