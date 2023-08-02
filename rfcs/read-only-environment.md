# Read-Only Environment

## Summary

Add a way to access a read-only version of the environment without disabling `safeenv`.

## Motivation

There are valid use cases for accessing the environment in a read-only way. As defined in the [RFC](https://github.com/Roblox/luau/blob/master/rfcs/deprecate-getfenv-setfenv.md) for deprecating 
getfenv and setfenv there are outlined cases where reading the environment is useful.

Increased performance can be achieved for transpilers written in Luau that may not be able to have a static environment created to access functions from or cant directly transpile 1:1. Similarly
interpreters written in Luau can benefit from being able to access functions without deoptimization, making other optimizations still active.

## Design

A safe way to access the environment is with a read-only table with `__index` and `__iter` metamethods.

a) `_ENV` table 

A read-only global table with metamethods that point to `LUA_ENVIRONINDEX`.

b) `getenv` or `getrenv` function 

Works identically to `getfenv` but only provides read-only access to the function's environment.

## Drawbacks

Type-checking can be difficult.

Roblox's marketplace currently hides assets that use `getfenv` so it would need to be updated to hide assets with `_ENV`/`getenv`/`getrenv`.

Environments can have metamethods of their own which may not interface well with a read-only `getenv`/`getrenv`.
