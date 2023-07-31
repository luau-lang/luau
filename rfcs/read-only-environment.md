# Read-Only Environment

## Summary

Add a way to access a read-only version of the environment without disabling `safeenv`.

## Motivation

There are valid use cases for accessing the environment in a read-only way. As defined in the [RFC](https://github.com/Roblox/luau/blob/master/rfcs/deprecate-getfenv-setfenv.md) for deprecating 
getfenv and setfenv there are outlined cases where reading the environment is useful.

Increased performance can be achieved for transpilers written in Luau that may not be able to have a static environment created to access functions from or cant directly transpile 1:1. Similarly
interpreters written in Luau can benefit from being able to access functions without deoptimization, making other optimizations still active.

...

## Design

...

## Drawbacks

...

## Alternatives

...
