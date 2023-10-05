# Byte Array Type

## Summary

A new built in type to serve as an array of bytes, with a library for reading and writing to the internal buffer. A particularly good example type that this could be derived from is [this Java class](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/nio/ByteBuffer.html).

## Motivation

With this type, we solve the use cases for binary format encoding and decoding, compression, and active/compact memory. This opens the door for developers to work with file formats that might've been too large to represent with tables or to write to strings. It also allows for writing algorithms that deal with raw data often, such as compression or hashing. Web services that exchange data in packed formats could also benefit from this.

## Design

This type will be called 'buffer' and will be implemented using `userdata` with a new reserved tag.

Operations on this type will be exposed through a new Luau library called 'buffer`, with the following functions:

`buffer.create(size: number): buffer`

Instantiates the object with a fixed size.
Each byte is initialized to 0.

'size' has to be an integer and it cannot be negative. Maximum size is defined by implementation, but it at least matches the maximum string size.

`buffer.fromstring(str: string): buffer`

Instantiates the object from a string.
Size of the buffer is fixed and equals to the length of the string.

`buffer.tostring(): string`

Returns the buffer data as a string.

`buffer.len(b: buffer): number`

Returns the size of the buffer.

`buffer.copy(target_buffer: buffer, target_offset: number, source_buffer: buffer, source_offset: number, count: number) -> ()`

Copy 'count' bytes from 'source_buffer' starting at offset 'source_offset' into the 'target_buffer' at 'target_offset'.

Offsets and 'count' have to be numbers, each number is cast to an integer in an implementation-defined way.

`buffer.readi8(b: buffer, offset: number): number`

`buffer.readu8(b: buffer, offset: number): number`

`buffer.readi16(b: buffer, offset: number): number`

`buffer.readu16(b: buffer, offset: number): number`

`buffer.readi32(b: buffer, offset: number): number`

`buffer.readu32(b: buffer, offset: number): number`

`buffer.readf32(b: buffer, offset: number): number`

`buffer.readf64(b: buffer, offset: number): number`

Used to read the data from the buffer by reinterpreting bytes at the offset as the type in the argument and converting it into a number.

Floating-point numbers are read from a format specified by IEEE 754.
When reading the value of any NaN representation, implementation can (but not required to) replace it with a different quiet NaN representation.

`buffer.writei8(b: buffer, offset: number, value: number): ()`

`buffer.writeu8(b: buffer, offset: number, value: number): ()`

`buffer.writei16(b: buffer, offset: number, value: number): ()`

`buffer.writeu16(b: buffer, offset: number, value: number): ()`

`buffer.writei32(b: buffer, offset: number, value: number): ()`

`buffer.writeu32(b: buffer, offset: number, value: number): ()`

`buffer.writef32(b: buffer, offset: number, value: number): ()`

`buffer.writef64(b: buffer, offset: number, value: number): ()`

Used to write data to the buffer by converting the number into the type specified by the argument and reinterpreting it as individual bytes.

Conversion to integer numbers performs a truncation of the number value. Results of converting special number values (inf/nan) is platform-specific.
Conversion to unsigned numbers uses `bit32` library semantics.

Floating-point numbers are stored in a format specified by IEEE 754.

`buffer.readstring(b: buffer, offset: number, count: number): string`

Used to read a string of length 'count' from the buffer at specified offset.

`buffer.writestring(b: buffer, offset: number, value: string, count: number?): ()`

Used to write data from a string into the buffer at specified offset.

If an optional 'count' is specified, only 'count' bytes are taken from the string. 'count' cannot be larger that the string length.

---

All offsets start at 0.

Read and write operations for relevant types are little endian as it is the most common use case, and conversion is often trivial to do manually.

Additionally, unaligned offsets in all operations are valid and behave as expected.

Unless otherwise specified, if a read or write operation would cause an access outside the data in the buffer, an error is thrown.

### Metatable

`buffer` also has a metatable, inside this metatable:
* '__type' is defined to return 'buffer'. `type()` will return 'userdata'
* '__eq' is defined to compare buffers, by comparing sizes first, followed by content comparison
* metatable is locked

No other metamethod is defined, naming a few specific onces:
* '__len' is not proposed at this time
* '__index' is not defined, so there is no `b[1] = a` interface to write bytes. Neither can you call library functions as methods like `b:writei16(10, 12)`
* '__iter' is not defined
* '__tostring' is not defined, generic userdata behavior remains, returning 'buffer: 0xpointer'
* ordering is not defined

## Drawbacks

This introduces 'buffer' as a class type in global typing context and adds new global 'buffer' table.
While class type might intersect with user-defined 'buffer' type, such type redefinitions ares already allowed in Luau, so this should not cause new type errors.
Same goes for the global table, users can already override globals like 'string', so additional of a new global is backwards-compatible, but new table will not be accessible in such a case.

Depending on implementation this could increase the complexity of the VM and related code. If this is to be implemented as a built-in, optimized type, it might need specialized fast paths for all relevant opcodes.

## Extensions

To support additional use cases, we can provide a set of `pushTYPE` and `takeTYPE` library functions and extend the type to have an internal cursor.
This will make it easy to write/read data from a buffer as one would from a file, without having to track the current offset manually.
Additional functions like `pos` and `setpos` can be added to access this internal cursor.

This extension can be made by changing the internal representation without affecting older code.

One drawback here might be that the cursor is attached to the data and raises a question if the value is preserved when object is serialized over the network.

---

Additional possibility will be to make the buffer change size automatically by `pushTYPE` interface. (explicit resize can be implemented with the existing interface).
This can also be changed almost transparently for older code.
One difference will be that `pushTYPE` will not throw when reaching the end of the data. Unless it is decided that other write operations could also resize implicitly.

Implementation can have a performance impact however as data will be read through a pointer redirection.

## Alternatives

The workarounds without this feature are significantly inefficient:

* Tables can, at most, represent 64 bits per slot using expensive `vector` packing.

* Tables with or without packing severely bloat memory, as each array entry is subject to Luau value size and alignment.

* Strings are immutable and can’t be used to efficiently construct binary data without exponential allocations.

* Built in `string.pack` and `string.unpack` can’t cover more complex schemas on their own or formats which are edited mid-creation.
