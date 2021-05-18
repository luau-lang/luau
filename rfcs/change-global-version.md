# Change \_VERSION global to "Luau"

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Change \_VERSION global to "Luau" to differentiate Luau from Lua

## Motivation

Provide an official way to distinguish Luau from Lua implementation.

## Design

We inherit the global string \_VERSION from Lua (this is distinct from Roblox `version()` function that returns a full version number such as 0.432.43589).

The string is set to "Lua 5.1" for us (and "Lua 5.2" etc for newer versions of Lua.

Since our implementation is sufficiently divergent from upstream, this proposal suggests setting \_VERSION to "Luau".
