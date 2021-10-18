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
unpack

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
