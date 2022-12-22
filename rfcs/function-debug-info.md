# debug.info

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add `debug.info` as programmatic debug info access API, similarly to Lua's `debug.getinfo`

## Motivation

Today Luau provides only one method to get the callstack, `debug.traceback`. This method traverses the entire stack and returns a string containing the call stack details - with no guarantees about the format of the call stack. As a result, the string doesn't present a formal API and can't be parsed programmatically.

There are a few cases where this can be inconvenient:

- Sometimes it is useful to pass the resulting call stack to some system expecting a structured input, e.g. for crash aggregation
- Sometimes it is useful to use the information about the caller for logging or filtering purposes; in these cases using just the script name can be useful, and getting script name out of the traceback is slow and imprecise

Additionally, in some cases instead of getting the information (such as script or function name) from the callstack, it can be useful to get it from a function object for diagnostic purposes. For example, maybe you want to call a callback and if it doesn't return expected results, display a user-friendly error message that contains the function name & script location - these aren't possible today at all.

## Design

The proposal is to expose a function from Lua standard library, `debug.getinfo`, to fix this problem - but change the function's signature for efficiency:

>   debug.info([thread], [function | level], options) -> any...

(note that the function has been renamed to make it more obvious that the behavior differs from that of Lua)

The parameters of the function match that of Lua's variant - the first argument is either a function object or a stack level (which is a number starting from 1, where 1 means "my caller"), or a thread (followed by the stack level), followed by a string that contains a list of things the result needs to contain:

   * s - function source identifier, in Roblox environment this is equal to the full name of the script the function is defined in
   * l - line number that the function is defined on (when examining a function) or line number of the stack frame (when examining a stack frame)
   * n - function name if present; this can be absent for anonymous functions or some C functions that don't have an assigned debug name
   * a - function arity information, which refers to the parameter count and whether the function is variadic or not
   * f - function object

Unlike Lua version, which would use the options given to fill a resulting table (e.g. "l" would map to a "currentline" and "linedefined" fields of the output table), our version will return the requested information in the order that it was requested in in the string - all letters specified above map to one extra returned value, "a" maps to a pair of a parameter number and a boolean indicating variadic status.

For example, here's how you implement a stack trace function:

```
   for i=1,100 do -- limit at 100 entries for very deep stacks
      local source, name, line = debug.info(i, "snl")
      if not source then break end
      if line >= 0 then
          print(string.format("%s(%d): %s", source, line, name or "anonymous"))
      else
          print(string.format("%s: %s", source, name or "anonymous"))
      end
   end
```

output:

```
   cs.lua(3): stacktrace
   cs.lua(17): bar
   cs.lua(13): foo
   [C]: pcall
   cs.lua(20): anonymous
```

When the first argument is a number and the input level is out of bounds, the function returns no values.

### Why the difference from Lua?

Lua's variant of this function has the same string as an input and the same thread/function/level combo as arguments before that, but returns a table with the requested data - or nil, when stack is exhausted.

The problem with this solution is performance. It results in generating excessive garbage by wrapping results in a table, which slows down the function call itself and generates extra garbage that needs to be collected later. This is not a problem for error handling scenarios, but can be an issue when logging is required; for example, `debug.info` with options containing a single result, "s" (mapping to source identifier aka script name), runs 3-4x slower when using a table variant with the current implementation of both functions in our VM.

While the difference in behavior is unfortunate, note that Lua has a long-standing precedent of using characters in strings to define the set of inputs or outputs for functions; of particular note is string.unpack which closely tracks this proposal where input string characters tell the implementation what data to return.

### Why not hardcode the options?

One possibility is that we could return all data associated with the function or a stack frame as a tuple.

This would work but has issues:

1. Because of the tuple-like API, the code becomes more error prone and less self-descriptive.
2. Some data is more expensive to access than other data - by forcing all callers to process all possible data we regress in performance; this is also why the original Lua API has an options string

To make sure we appropriately address 1, unlike Lua API in our API options string is mandatory to specify.

### Sandboxing risk?

Compared to information that you can already parse from traceback, the only extra data we expose is the function object. This is valuable when collecting stacks because retrieving the function object is faster than retrieving the associated source/name data - for example a very performant stack tracing implementation could collect data using "fl" (function and line number), and later when it comes the time to display the results, use `debug.info` again with "sn" to get script & name data from the object.

This technically wasn't possible to get before - this means in particular that if your function is ever called by another function, a malicious script could grab that function object again and call it with different arguments. However given that it's already possible to mutate global environment of any function on the callstack using getfenv/setfenv, the extra risk presented here seems minimal.

### Options delta from Lua

Lua presents the following options in getinfo:

* `n´	selects fields name and namewhat
*  `f´	selects field func
*  `S´	selects fields source, short_src, what, and linedefined
*  `l´	selects field currentline
*  `u´	selects field nup

We chose to omit `namewhat` as it's not meaningful in our implementation, omit `what` as it's redundant wrt source/short_src for C functions, replace source/short_src with only a single option (`s`) to avoid leaking script source via callstack API, remove `u` because there are no use cases for knowing the number of upvalues without debug.getupvalue API, and add `a` which has been requested by Roact team before for complex backwards compatibility workarounds wrt passed callbacks.

## Drawbacks

Having a different way to query debug information from Lua requires language-specific dispatch for code that wants to work on Lua and Luau.

## Alternatives

We could expose `debug.getinfo` from Lua as is; the problem is that in addition to performance issues highlighted above, Luau implementation doesn't track the same data and as such can't provide a fully compatible implementation short of implementing a shim for the sake of compatibility - an option this proposal keeps open.
