---
permalink: /library
title: Library
toc: true
---

Luau comes equipped with a standard library of functions designed to manipulate the built-in data types. Note that the library is relatively minimal and doesn't expose ways for
scripts to interact with the host environment - it's expected that embedding applications provide extra functionality on top of this and limit or sandbox the system access
appropriately, if necessary. For example, Roblox provides [a rich API to interact with the 3D environment and limited APIs to interact with external services](https://developer.roblox.com/en-us/api-reference).

This page documents the available builtin libraries and functions.

## Global functions

While most library functions are provided as part of a library like `table`, a few global functions are exposed without extra namespacing.

```
function assert<T>(value: T, message: string?): T
```

`assert` checks if the value is truthful; if it's not (which means it's `false` or `nil`), it throws an error. The error message can be customized with an optional parameter.
Upon success the function returns the `condition` argument.

```
function error<T>(message: T, level: number?)
```

`error` throws an error with the specified object. Note that errors don't have to be strings, although they often are by convention; various error handling mechanisms like `pcall`
preserve the error type. When `level` is specified, the error thrown is turned into a string that contains call frame information for the caller at level `level`, where `1` refers
to the function that called `error`. This can be useful to attribute the errors to callers, for example `error("Expected a valid object", 2)` highlights the caller of the function
that called `error` instead of the function itself in the callstack.

```
function gcinfo(): number
```

`gcinfo` returns the total heap size in kilobytes, which includes bytecode objects, global tables as well as the script-allocated objects. Note that Luau uses an incremental
garbage collector, and as such at any given point in time the heap may contain both reachable and unreachable objects. The number returned by `gcinfo` reflects the current heap
consumption from the operating system perspective and can fluctuate over time as garbage collector frees objects.

```
function getfenv(target: any?): table
```

getmetatable
next
newproxy
print
rawequal
rawget
rawset
select
setfenv
setmetatable
tonumber
tostring
type
typeof
ipairs
pairs
pcall
xpcall

