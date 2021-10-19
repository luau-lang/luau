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
function select(i: number | string, args: ...any): any
```

When called with `'#'` as the first argument, returns the number of remaining parameters passed. Otherwise, returns the parameter with the specified index.
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

Converts the input string to the number in base `base` (default 10) and returns the resulting number. If the conversion fails, returns `nil` instead.

```
function tostring(obj: any): string
```

Converts the input object to string and returns the result. If the object has a metatable with `__tostring` field, that method is called to perform the conversion.

```
function type(obj: any): string
```

Returns the type of the object; returns "userdata" for userdata objects.

```
function typeof(obj: any): string
```

Returns the type of the object; for userdata objects that have a metatable with the `__type` field, returns the value for that key.

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

Calls function `f` with parameters `args`. If the function suceeds, returns `true` followed by all return values of `f`. If the function throws an error, returns `false` followed by the error object.
Note that `f` can yield, which results in the entire coroutine yielding as well.

```
function xpcall(f: function, e: function, args: ...any): (boolean, ...any)
```

Calls function `f` with parameters `args`. If the function succeeds,  returns `true` followed by all return values of `f`. If the function throws an error, calls `e` with the error object as an argument, and returns `false` followed by all return values of `e`.
Note that `f` can yield, which results in the entire coroutine yielding as well.
`e` can neither yield nor error - if it does raise an error, `xpcall` returns with `false` followed by a special error message.

```
function unpack<V>(a: {V}, f: number?, t: number?): ...V
```

Returns all values of `a` with indices in `[f..t]` range. `f` defaults to 1 and `t` defaults to `#a`.

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

Returns the hyberbolic cosine of `n`.

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

Returns `n` if the number is in `[min, max]` range; otherwise, returns `min` when `n < min`, and `max` otherwise. The function errors if `min > max`.

```
function math.sign(n: number): number
```

Returns `-1` if `n` is negative, `1` if `n` is positive, and `0` if `n` is zero.

```
function math.round(n: number): number
```

Rounds `n` to the nearest integer boundary.

## table library

concat
foreach
foreachi
getn
maxn
insert
remove
sort
pack
unpack
move
create
find
clear
freeze
isfrozen

## string library

byte
char
find
format
gmatch
gsub
len
lower
match
rep
reverse
sub
upper
split
pack
packsize
unpack

## coroutine library

create
running
status
wrap
yield
isyieldable
resume

## bit32 library

arshift
band
bnot
bor
bxor
btest
extract
lrotate
lshift
replace
rrotate
rshift

## utf8 library

offset
codepoint
char
len
codes

## os library

clock
date
difftime
time

## debug library

info
traceback
