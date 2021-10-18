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
function abs(n: number): number
```

Returns the absolute value of `n`. Returns NaN if the input is NaN. 

```
function acos(n: number): number
```

Returns the arc cosine of `n`, expressed in radians. Returns a value in `[0, pi]` range. Returns NaN if the input is not in `[-1, +1]` range.

```
function asin(n: number): number
```

Returns the arc sine of `n`, expressed in radians. Returns a value in `[-pi/2, +pi/2]` range. Returns NaN if the input is not in `[-1, +1]` range.

```
function atan2(y: number, x: number): number
```

Returns the arc tangent of `y/x`, expressed in radians. The function takes into account the sign of both arguments in order to determine the quadrant. Returns a value in `[-pi, pi]` range.

```
function atan(n: number): number
```

Returns the arc tangent of `n`, expressed in radians. Returns a value in `[-pi/2, pi-2]` range.

```
function ceil(n: number): number
```

Rounds `n` upwards to the next integer boundary.

```
function cosh(n: number): number
```

Returns the hyberbolic cosine of `n`.

```
function cos(n: number): number
```

Returns the cosine of `n`, which is an angle in radians. Returns a value in `[0, 1]` range.

```
function deg(n: number): number
```

Converts `n` from radians to degrees and returns the result.

```
function exp(n: number): number
```

Returns the base-e exponent of `n`, that is `e^n`.

```
function floor(n: number): number
```

Rounds `n` downwards to previous integer boundary.

fmod
frexp
ldexp
log10
log
max
min
modf
pow
rad
random
randomseed
sinh
sin
sqrt
tanh
tan
noise
clamp
sign
round

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
