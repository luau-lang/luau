---
permalink: /library
title: Library
toc: true
---

Luau comes equipped with a standard library of functions designed to manipulate the built-in data types. Note that the library is relatively minimal and doesn't expose ways for
scripts to interact with the host environment - it's expected that embedding applications provide extra functionality on top of this and limit or sandbox the system access
appropriately, if necessary. For example, Roblox provides [a rich API to interact with the 3D environment and limited APIs to interact with external services](https://developer.roblox.com/en-us/api-reference).

This page documents the available builtin libraries and functions. All of these are accessible by default by any script, assuming the host environment exposes them (which is usually a safe assumption outside of extremely constrained environments).

## Global functions

While most library functions are provided as part of a library like `table`, a few global functions are exposed without extra namespacing.

```
function assert<T>(value: T, message: string?): T
```

`assert` checks if the value is truthy; if it's not (which means it's `false` or `nil`), it raises an error. The error message can be customized with an optional parameter.
Upon success the function returns the `value` argument.

```
function error(obj: any, level: number?)
```

`error` raises an error with the specified object. Note that errors don't have to be strings, although they often are by convention; various error handling mechanisms like `pcall`
preserve the error type. When `level` is specified, the error raised is turned into a string that contains call frame information for the caller at level `level`, where `1` refers
to the function that called `error`. This can be useful to attribute the errors to callers, for example `error("Expected a valid object", 2)` highlights the caller of the function
that called `error` instead of the function itself in the callstack.

```
function gcinfo(): number
```

`gcinfo` returns the total heap size in kilobytes, which includes bytecode objects, global tables as well as the script-allocated objects. Note that Luau uses an incremental
garbage collector, and as such at any given point in time the heap may contain both reachable and unreachable objects. The number returned by `gcinfo` reflects the current heap
consumption from the operating system perspective and can fluctuate over time as garbage collector frees objects.

```
function getfenv(target: (function | number)?): table
```

Returns the environment table for target function; when `target` is not a function, it must be a number corresponding to the caller stack index, where 1 means the function that calls `getfenv`, and the environment table is returned for the corresponding function from the call stack. When `target` is omitted it defaults to `1`, so `getfenv()` returns the environment table for the calling function.

```
function getmetatable(obj: any): table?
```

Returns the metatable for the specified object; when object is not a table or a userdata, the returned metatable is shared between all objects of the same type. Note that when metatable is protected (has a `__metatable` key), the value corresponding to that key is returned instead and may not be a table.

```
function next<K, V>(t: { [K]: V }, i: K?): (K, V)?
```

Given the table `t`, returns the next key-value pair after `i` in the table traversal order, or nothing if `i` is the last key. When `i` is `nil`, returns the first key-value pair instead.

```
function newproxy(mt: boolean?): userdata
```

Creates a new untyped userdata object; when `mt` is true, the new object has an empty metatable that can be modified using `getmetatable`.

```
function print(args: ...any)
```

Prints all arguments to the standard output, using Tab as a separator.

```
function rawequal(a: any, b: any): boolean
```

Returns true iff `a` and `b` have the same type and point to the same object (for garbage collected types) or are equal (for value types).

```
function rawget<K, V>(t: { [K]: V }, k: K): V?
```

Performs a table lookup with index `k` and returns the resulting value, if present in the table, or nil. This operation bypasses metatables/`__index`.

```
function rawset<K, V>(t: { [K] : V }, k: K, v: V)
```

Assigns table field `k` to the value `v`. This operation bypasses metatables/`__newindex`.

```
function select<T>(i: string, args: ...T): number
function select<T>(i: number, args: ...T): ...T
```

When called with `'#'` as the first argument, returns the number of remaining parameters passed. Otherwise, returns the subset of parameters starting with the specified index.
Index can be specified from the start of the arguments (using 1 as the first argument), or from the end (using -1 as the last argument).

```
function setfenv(target: function | number, env: table)
```

Changes the environment table for target function to `env`; when `target` is not a function, it must be a number corresponding to the caller stack index, where 1 means the function that calls `setfenv`, and the environment table is returned for the corresponding function from the call stack.

```
function setmetatable(t: table, mt: table?)
```

Changes metatable for the given table. Note that unlike `getmetatable`, this function only works on tables. If the table already has a protected metatable (has a `__metatable` field), this function errors.

```
function tonumber(s: string, base: number?): number?
```

Converts the input string to the number in base `base` (default 10) and returns the resulting number. If the conversion fails (that is, if the input string doesn't represent a valid number in the specified base), returns `nil` instead.

```
function tostring(obj: any): string
```

Converts the input object to string and returns the result. If the object has a metatable with `__tostring` field, that method is called to perform the conversion.

```
function type(obj: any): string
```

Returns the type of the object, which is one of `"nil"`, `"boolean"`, `"number"`, `"vector"`, `"string"`, `"table"`, `"function"`, `"userdata"` or `"thread"`.

```
function typeof(obj: any): string
```

Returns the type of the object; for userdata objects that have a metatable with the `__type` field *and* are defined by the host with an internal tag, returns the value for that key.
For custom userdata objects, such as ones returned by `newproxy`, this function returns `"userdata"` to make sure host-defined types can not be spoofed.

```
function ipairs(t: table): <iterator>
```

Returns the triple (generator, state, nil) that can be used to traverse the table using a `for` loop. The traversal results in key-value pairs for the numeric portion of the table; key starts from 1 and increases by 1 on each iteration. The traversal terminates when reaching the first `nil` value (so `ipairs` can't be used to traverse array-like tables with holes).

```
function pairs(t: table): <iterator>
```

Returns the triple (generator, state, nil) that can be used to traverse the table using a `for` loop. The traversal results in key-value pairs for all keys in the table, numeric and otherwise, but doesn't have a defined order.

```
function pcall(f: function, args: ...any): (boolean, ...any)
```

Calls function `f` with parameters `args`. If the function succeeds, returns `true` followed by all return values of `f`. If the function raises an error, returns `false` followed by the error object.
Note that `f` can yield, which results in the entire coroutine yielding as well.

```
function xpcall(f: function, e: function, args: ...any): (boolean, ...any)
```

Calls function `f` with parameters `args`. If the function succeeds,  returns `true` followed by all return values of `f`. If the function raises an error, calls `e` with the error object as an argument, and returns `false` followed by all return values of `e`.
Note that `f` can yield, which results in the entire coroutine yielding as well.
`e` can neither yield nor error - if it does raise an error, `xpcall` returns with `false` followed by a special error message.

```
function unpack<V>(a: {V}, f: number?, t: number?): ...V
```

Returns all values of `a` with indices in `[f..t]` range. `f` defaults to 1 and `t` defaults to `#a`. Note that this is equivalent to `table.unpack`.

## math library

```
function math.abs(n: number): number
```

Returns the absolute value of `n`. Returns NaN if the input is NaN. 

```
function math.acos(n: number): number
```

Returns the arc cosine of `n`, expressed in radians. Returns a value in `[0, pi]` range. Returns NaN if the input is not in `[-1, +1]` range.

```
function math.asin(n: number): number
```

Returns the arc sine of `n`, expressed in radians. Returns a value in `[-pi/2, +pi/2]` range. Returns NaN if the input is not in `[-1, +1]` range.

```
function math.atan2(y: number, x: number): number
```

Returns the arc tangent of `y/x`, expressed in radians. The function takes into account the sign of both arguments in order to determine the quadrant. Returns a value in `[-pi, pi]` range.

```
function math.atan(n: number): number
```

Returns the arc tangent of `n`, expressed in radians. Returns a value in `[-pi/2, pi-2]` range.

```
function math.ceil(n: number): number
```

Rounds `n` upwards to the next integer boundary.

```
function math.cosh(n: number): number
```

Returns the hyperbolic cosine of `n`.

```
function math.cos(n: number): number
```

Returns the cosine of `n`, which is an angle in radians. Returns a value in `[0, 1]` range.

```
function math.deg(n: number): number
```

Converts `n` from radians to degrees and returns the result.

```
function math.exp(n: number): number
```

Returns the base-e exponent of `n`, that is `e^n`.

```
function math.floor(n: number): number
```

Rounds `n` downwards to previous integer boundary.

```
function math.fmod(x: number, y: number): number
```

Returns the remainder of `x` modulo `y`, rounded towards zero. Returns NaN if `y` is zero.

```
function math.frexp(n: number): (number, number)
```

Splits the number into a significand (a number in `[-1, +1]` range) and binary exponent such that `n = s * 2^e`, and returns `s, e`.

```
function math.ldexp(s: number, e: number): number
```

Given the significand and a binary exponent, returns a number `s * 2^e`.

```
function math.log10(n: number): number
```

Returns base-10 logarithm of the input number. Returns NaN if the input is negative, and negative infinity if the input is 0.
Equivalent to `math.log(n, 10)`.

```
function math.log(n: number, base: number?): number
```

Returns logarithm of the input number in the specified base; base defaults to `e`. Returns NaN if the input is negative, and negative infinity if the input is 0.

```
function math.max(list: ...number): number
```

Returns the maximum number of the input arguments. The function requires at least one input and will error if zero parameters are passed. If one of the inputs is a NaN, the result may or may not be a NaN.

```
function math.min(list: ...number): number
```

Returns the minimum number of the input arguments. The function requires at least one input and will error if zero parameters are passed. If one of the inputs is a NaN, the result may or may not be a NaN.

```
function math.modf(n: number): (number, number)
```

Returns the integer and fractional part of the input number. Both the integer and fractional part have the same sign as the input number, e.g. `math.modf(-1.5)` returns `-1, -0.5`.

```
function math.pow(x: number, y: number): number
```

Returns `x` raised to the power of `y`.

```
function math.rad(n: number): number
```

Converts `n` from degrees to radians and returns the result.

```
function math.random(): number
function math.random(n: number): number
function math.random(min: number, max: number): number
```

Returns a random number using the global random number generator. A zero-argument version returns a number in `[0, 1]` range. A one-argument version returns a number in `[1, n]` range. A two-argument version returns a number in `[min, max]` range. The input arguments are truncated to integers, so `math.random(1.5)` always returns 1.

```
function math.randomseed(seed: number)
```

Reseeds the global random number generator; subsequent calls to `math.random` will generate a deterministic sequence of numbers that only depends on `seed`.

```
function math.sinh(n: number): number
```

Returns a hyperbolic sine of `n`.

```
function math.sin(n: number): number
```

Returns the sine of `n`, which is an angle in radians. Returns a value in `[0, 1]` range.

```
function math.sqrt(n: number): number
```

Returns the square root of `n`. Returns NaN if the input is negative.

```
function math.tanh(n: number): number
```

Returns the hyperbolic tangent of `n`.

```
function math.tan(n: number): number
```

Returns the tangent of `n`, which is an angle in radians.

```
function math.noise(x: number, y: number?, z: number?): number
```

Returns 3D Perlin noise value for the point `(x, y, z)` (`y` and `z` default to zero if absent). Returns a value in `[-1, 1]` range.

```
function math.clamp(n: number, min: number, max: number): number
```

Returns `n` if the number is in `[min, max]` range; otherwise, returns `min` when `n < min`, and `max` otherwise. If `n` is NaN, may or may not return NaN.
The function errors if `min > max`.

```
function math.sign(n: number): number
```

Returns `-1` if `n` is negative, `1` if `n` is positive, and `0` if `n` is zero or NaN.

```
function math.round(n: number): number
```

Rounds `n` to the nearest integer boundary.

## table library

```
function table.concat(a: {string}, sep: string?, f: number?, t: number?): string
```

Concatenate all elements of `a` with indices in range `[f..t]` together, using `sep` as a separator if present. `f` defaults to 1 and `t` defaults to `#a`.

```
function table.foreach<K, V, R>(t: { [K]: V }, f: (K, V) -> R?): R?
```

Iterates over all elements of the table in unspecified order; for each key-value pair, calls `f` and returns the result of `f` if it's non-nil. If all invocations of `f` returned `nil`, returns no values.

```
function table.foreachi<V, R>(t: {V}, f: (number, V) -> R?): R?
```

Iterates over numeric keys of the table in `[1..#t]` range in order; for each key-value pair, calls `f` and returns the result of `f` if it's non-nil. If all invocations of `f` returned `nil`, returns no values.

```
function table.getn<V>(t: {V}): number
```

Returns the length of table `t` (equivalent to `#t`).

```
function table.maxn<V>(t: {V}): number
```

Returns the maximum numeric key of table `t`, or zero if the table doesn't have numeric keys.

```
function table.insert<V>(t: {V}, v: V)
function table.insert<V>(t: {V}, i: number, v: V)
```

When using a two-argument version, appends the value to the array portion of the table (equivalent to `t[#t+1] = v`).
When using a three-argument version, inserts the value at index `i` and shifts values at indices after that by 1. `i` should be in `[1..#t]` range.

```
function table.remove<V>(t: {V}, i: number?)
```

Removes element `i` from the table and shifts values at indices after that by 1. If `i` is not specified, removes the last element of the table.
`i` should be in `[1..#t]` range.

```
function table.sort<V>(t: {V}, f: ((V, V) -> boolean)?)
```

Sorts the table `t` in ascending order, using `f` as a comparison predicate: `f` should return `true` iff the first parameter should be before the second parameter in the resulting table. When `f` is not specified, builtin less-than comparison is used instead.
The comparison predicate must establish a strict weak ordering - sort results are undefined otherwise.

```
function table.pack<V>(args: ...V): { [number]: V, n: number }
```

Returns a table that consists of all input arguments as array elements, and `n` field that is set to the number of inputs.

```
function table.unpack<V>(a: {V}, f: number?, t: number?): ...V
```

Returns all values of `a` with indices in `[f..t]` range. `f` defaults to 1 and `t` defaults to `#a`.

```
function table.move<V>(a: {V}, f: number, t: number, d: number, tt: {V}?)
```

Copies elements in range `[f..t]` from table `a` to table `tt` if specified and `a` otherwise, starting from the index `d`.

```
function table.create<V>(n: number, v: V?): {V}
```

Creates a table with `n` elements; all of them (range `[1..n]`) are set to `v`. When `v` is nil or omitted, the returned table is empty but has preallocated space for `n` elements which can make subsequent insertions faster.
Note that preallocation is only performed for the array portion of the table - using `table.create` on dictionaries is counter-productive.

```
function table.find<V>(t: {V}, v: V): number?
```

Find the first element in the table that is equal to `v` and returns its index; the traversal stops at the first `nil`. If the element is not found, `nil` is returned instead.

```
function table.clear(t: table)
```

Removes all elements from the table while preserving the table capacity, so future assignments don't need to reallocate space.

```
function table.freeze(t: table): table
```

Given a non-frozen table, freezes it such that all subsequent attempts to modify the table or assign its metatable raise an error. If the input table is already frozen or has a protected metatable, the function raises an error; otherwise it returns the input table.
Note that the table is frozen in-place and is not being copied. Additionally, only `t` is frozen, and keys/values/metatable of `t` don't change their state and need to be frozen separately if desired.

```
function table.isfrozen(t: table): boolean
```

Returns `true` iff the input table is frozen.

## string library

```
function string.byte(s: string, f: number?, t: number?): ...number
```

Returns the numeric code of every byte in the input string with indices in range `[f..t]`. `f` defaults to 1 and `t` defaults to `f`, so a two-argument version of this function returns a single number. If the function is called with a single argument and the argument is out of range, the function returns no values.

```
function string.char(args: ...number): string
```

Returns the string that contains a byte for every input number; all inputs must be integers in `[0..255]` range.

```
function string.find(s: string, p: string, init: number?, plain: boolean?): (number?, number?)
```

Tries to find an instance of pattern `p` in the string `s`, starting from position `init` (defaults to 1). When `plain` is true, the search is using raw case-insensitive string equality, otherwise `p` should be a [string pattern](https://www.lua.org/manual/5.3/manual.html#6.4.1). If a match is found, returns the position of the match and the length of the match, followed by the pattern captures; otherwise returns `nil`.

```
function string.format(s: string, args: ...any): string
```

Returns a formatted version of the input arguments using a [printf-style format string](https://en.cppreference.com/w/c/io/fprintf) `s`. The following format characters are supported:

- `c`: expects an integer number and produces a character with the corresponding character code
- `d`, `i`, `u`: expects an integer number and produces the decimal representation of that number
- `o`: expects an integer number and produces the octal representation of that number
- `x`, `X`: expects an integer number and produces the hexadecimal representation of that number, using lower case or upper case hexadecimal characters
- `e`, `E`, `f`, `g`, `G`: expects a number and produces the floating point representation of that number, using scientific or decimal representation
- `q`: expects a string and produces the same string quoted using double quotation marks, with escaped special characters if necessary
- `s`: expects a string and produces the same string verbatim

The formats support modifiers `-`, `+`, ` `, `#` and `0`, as well as field width and precision modifiers - with the exception of `*`.

```
function string.gmatch(s: string, p: string): <iterator>
```

Produces an iterator function that, when called repeatedly explicitly or via `for` loop, produces matches of string `s` with [string pattern](https://www.lua.org/manual/5.3/manual.html#6.4.1) `p`. For every match, the captures within the pattern are returned if present (if a pattern has no captures, the entire matching substring is returned instead).

```
function string.gsub(s: string, p: string, f: function | table | string, maxs: number?): (string, number)
```

For every match of [string pattern](https://www.lua.org/manual/5.3/manual.html#6.4.1) `p` in `s`, replace the match according to `f`. The substitutions stop after the limit of `maxs`, and the function returns the resulting string followed by the number of substitutions.

When `f` is a string, the substitution uses the string as a replacement. When `f` is a table, the substitution uses the table element with key corresponding to the first pattern capture, if present, and entire match otherwise. Finally, when `f` is a function, the substitution uses the result of calling `f` with call pattern captures, or entire matching substring if no captures are present.

```
function string.len(s: string): number
```

Returns the number of bytes in the string (equivalent to `#s`).

```
function string.lower(s: string): string
```

Returns a string where each byte corresponds to the lower-case ASCII version of the input byte in the source string.

```
function string.match(s: string, p: string, init: number?): (number?, number?)
```

Tries to find an instance of pattern `p` in the string `s`, starting from position `init` (defaults to 1). `p` should be a [string pattern](https://www.lua.org/manual/5.3/manual.html#6.4.1). If a match is found, returns all pattern captures, or entire matching substring if no captures are present, otherwise returns `nil`.

```
function string.rep(s: string, n: number): string
```

Returns the input string `s` repeated `n` times. Returns an empty string if `n` is zero or negative.

```
function string.reverse(s: string): string
```

Returns the string with the order of bytes reversed compared to the original. Note that this only works if the input is a binary or ASCII string.

```
function string.sub(s: string, f: number, t: number?): string
```

Returns a substring of the input string with the byte range `[f..t]`; `t` defaults to `#s`, so a two-argument version returns a string suffix.

```
function string.upper(s: string): string
```

Returns a string where each byte corresponds to the upper-case ASCII version of the input byte in the source string.

```
function string.split(s: string, sep: string?): {string}
```

Splits the input string using `sep` as a separator (defaults to `","`) and returns the resulting substrings. If separator is empty, the input string is split into separate one-byte strings.

```
function string.pack(f: string, args: ...any): string
```

Given a [pack format string](https://www.lua.org/manual/5.3/manual.html#6.4.2), encodes all input parameters according to the packing format and returns the resulting string. Note that Luau uses fixed sizes for all types that have platform-dependent size in Lua 5.x: short is 16 bit, long is 64 bit, integer is 32-bit and size_t is 32 bit for the purpose of string packing.

```
function string.packsize(f: string): number
```

Given a [pack format string](https://www.lua.org/manual/5.3/manual.html#6.4.2), returns the size of the resulting packed representation. The pack format can't use variable-length format specifiers. Note that Luau uses fixed sizes for all types that have platform-dependent size in Lua 5.x: short is 16 bit, long is 64 bit, integer is 32-bit and size_t is 32 bit for the purpose of string packing.

```
function string.unpack(f: string, s: string): ...any
```

Given a [pack format string](https://www.lua.org/manual/5.3/manual.html#6.4.2), decodes the input string according to the packing format and returns all resulting values. Note that Luau uses fixed sizes for all types that have platform-dependent size in Lua 5.x: short is 16 bit, long is 64 bit, integer is 32-bit and size_t is 32 bit for the purpose of string packing.

## coroutine library

```
function coroutine.create(f: function): thread
```

Returns a new coroutine that, when resumed, will run function `f`.

```
function coroutine.running(): thread?
```

Returns the currently running coroutine, or `nil` if the code is running in the main coroutine (depending on the host environment setup, main coroutine may never be used for running code).

```
function coroutine.status(co: thread): string
```

Returns the status of the coroutine, which can be `"running"`, `"suspended"`, `"normal"` or `"dead"`. Dead coroutines have finished their execution and can not be resumed, but their state can still be inspected as they are not dead from the garbage collector point of view.

```
function coroutine.wrap(f: function): function
```

Creates a new coroutine and returns a function that, when called, resumes the coroutine and passes all arguments along to the suspension point. When the coroutine yields or finishes, the wrapped function returns with all values returned at the suspension point.

```
function coroutine.yield(args: ...any): ...any
```

Yields the currently running coroutine and passes all arguments along to the code that resumed the coroutine. The coroutine becomes suspended; when the coroutine is resumed again, the resumption arguments will be forwarded to `yield` which will behave as if it returned all of them.

```
function coroutine.isyieldable(): boolean
```

Returns `true` iff the currently running coroutine can yield. Yielding is prohibited when running inside metamethods like `__index` or C functions like `table.foreach` callback, with the exception of `pcall`/`xpcall`.

```
function coroutine.resume(co: thread, args: ...any): (boolean, ...any)
```

Resumes the coroutine and passes the arguments along to the suspension point. When the coroutine yields or finishes, returns `true` and all values returned at the suspension point. If an error is raised during coroutine resumption, this function returns `false` and the error object, similarly to `pcall`.

```
function coroutine.close(co: thread): (boolean, any?)
```

Closes the coroutine which puts coroutine in the dead state. The coroutine must be dead or suspended - in particular it can't be currently running. If the coroutine that's being closed was in an error state, returns `false` along with an error object; otherwise returns `true`. After closing, the coroutine can't be resumed and the coroutine stack becomes empty.

## bit32 library

All functions in the `bit32` library treat input numbers as 32-bit unsigned integers in `[0..4294967295]` range. The bit positions start at 0 where 0 corresponds to the least significant bit.

```
function bit32.arshift(n: number, i: number): number
```

Shifts `n` by `i` bits to the right (if `i` is negative, a left shift is performed instead). The most significant bit of `n` is propagated during the shift.

```
function bit32.band(args: ...number): number
```

Performs a bitwise `and` of all input numbers and returns the result. If the function is called with no arguments, an integer with all bits set to 1 is returned.

```
function bit32.bnot(n: number): number
```

Returns a bitwise negation of the input number.

```
function bit32.bor(args: ...number): number
```

Performs a bitwise `or` of all input numbers and returns the result. If the function is called with no arguments, zero is returned.

```
function bit32.bxor(args: ...number): number
```

Performs a bitwise `xor` (exclusive or) of all input numbers and returns the result. If the function is called with no arguments, zero is returned.

```
function bit32.btest(args: ...number): boolean
```

Perform a bitwise `and` of all input numbers, and return `true` iff the result is not 0. If the function is called with no arguments, `true` is returned.

```
function bit32.extract(n: number, f: number, w: number?): number
```

Extracts bits at positions `[f..w]` and returns the resulting integer. `w` defaults to `f+1`, so a two-argument version of `extract` returns the bit value at position `f`.

```
function bit32.lrotate(n: number, i: number): number
```

Rotates `n` to the left by `i` bits (if `i` is negative, a right rotate is performed instead); the bits that are shifted past the bit width are shifted back from the right.

```
function bit32.lshift(n: number, i: number): number
```

Shifts `n` to the left by `i` bits (if `i` is negative, a right shift is performed instead).

```
function bit32.replace(n: number, r: number, f: number, w: number?): number
```

Replaces bits at positions `[f..w]` of number `n` with `r` and returns the resulting integer. `w` defaults to `f+1`, so a three-argument version of `replace` changes one bit at position `f` to `r` (which should be 0 or 1) and returns the result.

```
function bit32.rrotate(n: number, i: number): number
```

Rotates `n` to the right by `i` bits (if `i` is negative, a left rotate is performed instead); the bits that are shifted past the bit width are shifted back from the left.

```
function bit32.rshift(n: number, i: number): number
```

Shifts `n` to the right by `i` bits (if `i` is negative, a left shift is performed instead).

```
function bit32.countlz(n: number): number
```

Returns the number of consecutive zero bits in the 32-bit representation of `n` starting from the left-most (most significant) bit. Returns 32 if `n` is zero.

```
function bit32.countrz(n: number): number
```

Returns the number of consecutive zero bits in the 32-bit representation of `n` starting from the right-most (least significant) bit. Returns 32 if `n` is zero.

## utf8 library

Strings in Luau can contain arbitrary bytes; however, in many applications strings representing text contain UTF8 encoded data by convention, that can be inspected and manipulated using `utf8` library.

```
function utf8.offset(s: string, n: number, i: number?): number?
```

Returns the byte offset of the Unicode codepoint number `n` in the string, starting from the byte position `i`. When the character is not found, returns `nil` instead.

```
function utf8.codepoint(s: string, i: number?, j: number?): ...number
```

Returns a number for each Unicode codepoint in the string with the starting byte offset in `[i..j]` range. `i` defaults to 1 and `j` defaults to `i`, so a two-argument version of this function returns the Unicode codepoint that starts at byte offset `i`.

```
function utf8.char(args: ...number): string
```

Creates a string by concatenating Unicode codepoints for each input number.

```
function utf8.len(s: string, i: number?, j: number?): number?
```

Returns the number of Unicode codepoints with the starting byte offset in `[i..j]` range, or `nil` followed by the first invalid byte position if the input string is malformed.
`i` defaults to 1 and `j` defaults to `#s`, so `utf8.len(s)` returns the number of Unicode codepoints in string `s` or `nil` if the string is malformed.

```
function utf8.codes(s: string): <iterator>
```

Returns an iterator that, when used in `for` loop, produces the byte offset and the codepoint for each Unicode codepoints that `s` consists of.

## os library

```
function os.clock(): number
```

Returns a high-precision timestamp (in seconds) that doesn't have a defined baseline, but can be used to measure duration with sub-microsecond precision.

```
function os.date(s: string?, t: number?): table | string
```

Returns the table or string representation of the time specified as `t` (defaults to current time) according to `s` format string.

When `s` starts with `!`, the result uses UTC, otherwise it uses the current timezone.

If `s` is equal to `*t` (or `!*t`), a table representation of the date is returned, with keys `sec`/`min`/`hour` for the time (using 24-hour clock), `day`/`month`/`year` for the date, `wday` for week day (1..7), `yday` for year day (1..366) and `isdst` indicating whether the timezone is currently using daylight savings.

Otherwise, `s` is interpreted as a [date format string](https://www.cplusplus.com/reference/ctime/strftime/), with the valid specifiers including any of `aAbBcdHIjmMpSUwWxXyYzZ` or `%`. `s` defaults to `"%c"` so `os.date()` returns the human-readable representation of the current date in local timezone.

```
function os.difftime(a: number, b: number): number
```

Calculates the difference in seconds between `a` and `b`; provided for compatibility only. Please use `a - b` instead.

```
function os.time(t: table?): number
```

When called without arguments, returns the current date/time as a Unix timestamp. When called with an argument, expects it to be a table that contains `sec`/`min`/`hour`/`day`/`month`/`year` keys and returns the Unix timestamp of the specified date/time in UTC.

## debug library

```
function debug.info(co: thread, level: number, s: string): ...any
function debug.info(level: number, s: string): ...any
function debug.info(f: function, s: string): ...any
```

Given a stack frame or a function, and a string that specifies the requested information, returns the information about the stack frame or function.

Each character of `s` results in additional values being returned in the same order as the characters appear in the string:

- `s` returns source path for the function
- `l` returns the line number for the stack frame or the line where the function is defined when inspecting a function object
- `n` returns the name of the function, or an empty string if the name is not known
- `f` returns the function object itself
- `a` returns the number of arguments that the function expects followed by a boolean indicating whether the function is variadic or not

For example, `debug.info(2, "sln")` returns source file, current line and function name for the caller of the current function.

```
function debug.traceback(co: thread, msg: string?, level: number?): string
function debug.traceback(msg: string?, level: number?): string
```

Produces a stringified callstack of the given thread, or the current thread, starting with level `level`. If `msg` is specified, then the resulting callstack includes the string before the callstack output, separated with a newline. The format of the callstack is human-readable and subject to change.
