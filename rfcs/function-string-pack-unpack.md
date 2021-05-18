# string.pack/unpack/packsize from Lua 5.3

> Note: this RFC was adapted from an internal proposal that predates RFC process

**Status**: Implemented

## Summary

Add string pack/unpack from Lua 5.3 for binary interop, with small tweaks to format specification to make format strings portable.

## Motivation

While the dominant usecase for Luau is a game programming language, for backend work it's sometimes the case that developers need to work with formats defined outside of Roblox. When these are structured as JSON, it's easy, but if they are binary, it's not. Additionally for the game programming, often developers end up optimizing their data transmission using custom binary codecs where they know the range of the data (e.g. it's much more efficient to send a number using 1 byte if you know the number is between 0 and 1 and 8 bits is enough, but RemoteEvent/etc won't do it for you because it guarantees lossless roundtrip). For both working with external data and optimizing data transfer, it would be nice to have a way to work with binary data.

This is doable in Luau using `string.byte`/`string.char`/`bit32` library/etc. but tends to be a bit cumbersome. Lua 5.3 provides functions `string.pack`/`string.unpack`/`string.packsize` that, while not solving 100% of the problems, often make working with binary much easier and much faster. This proposal suggests adding them to Luau - this will both further our goal to be reasonably compatible with latest Lua versions, and make it easier for developers to write some types of code.

## Design

Concretely, this proposal suggests adding the following functions:

```
string.pack (fmt, v1, v2, ···)
```

Returns a binary string containing the values v1, v2, etc. packed (that is, serialized in binary form) according to the format string fmt.

```
string.packsize (fmt)
```

Returns the size of a string resulting from string.pack with the given format. The format string cannot have the variable-length options 's' or 'z'.

```
string.unpack (fmt, s [, pos])
```

Returns the values packed in string s (see string.pack) according to the format string fmt. An optional pos marks where to start reading in s (default is 1). After the read values, this function also returns the index of the first unread byte in s.

The format string is a sequence of characters that define the data layout that is described here in full: https://www.lua.org/manual/5.3/manual.html#6.4.2. We will adopt this wholesale, but we will guarantee that the resulting code is cross-platform by:

a) Ensuring native endian is little endian (de-facto true for all our platforms)
b) Fixing sizes of native formats to 2b short, 4b int, 8b long
c) Treating `size_t` in context of `T` and `s` formats as a 32-bit integer

Of course, the functions are memory-safe; if the input string is too short to provide all relevant data they will fail with "data string is too short" error.

This may seem slightly unconventional but it's very powerful and expressive, in much the same way format strings and regular expressions are :) Here's a basic example of how you might transmit a 3-component vector with this:

```
-- returns a 24-byte string with 64-bit double encoded three times, similar to how we'd replicate 3 raw numbers
string.pack("ddd", x, y, z)

-- returns a 12-byte string with 32-bit float encoded three times, similar to how we'd replicate Vector3
string.pack("fff", x, y, z)

-- returns a 3-byte string with each value stored in 8 bits
-- assumes -1..1 range; this code doesn't round the right way because I'm too lazy
string.pack("bbb", x * 127, y * 127, z * 127)
```

The unpacking of the data is symmetrical - using the same format string and `string.unpack` you get the encoded data back.

## Drawbacks

The format specification is somewhat arbitrary and is likely to be unfamiliar to people who come with prior experience in other languages (having said that, this feature closely follows equivalent functionality from Ruby).

The implementation of string pack/unpack requires yet another format string matcher, which increases complexity of the builtin libraries and static analysis (since we need to provide linting for another format string syntax).

## Alternatives

We could force developers to rely on existing functionality for string packing; it is possible to replicate this proposal in a library, although at a much reduced performance.
