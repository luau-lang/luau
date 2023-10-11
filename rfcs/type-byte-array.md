# Byte Array Type

## Summary

A new built in type to serve as a mutable array of bytes, with a library for reading and writing the contents.

## Motivation

Existing mechanisms for representing binary data in Luau can be insufficient for performance-oriented use cases.

A binary blob may be represented as an array of numbers 0-255 (idiomatic and reasonably performant, but very space-inefficient: each element takes 16 bytes, and it's difficult to work with data that is wider than bytes) or a string (only works for read-only cases, data extraction is possible via `string.unpack` but not very efficient). Neither of the two options are optimal, especially when the use case is data encoding (as opposed to decoding).

While the host can provide custom data types that close this gap using `userdata` with overridden `__index`/`__newindex` that provide byte storage, the resulting type would be memory-efficient but not performance-efficient due to the cost of metamethod dispatch for every access. Additionally, since every host has a different API, this would make it difficult to write portable Luau algorithms that require efficient binary access.

With this type, we solve the use cases for binary format encoding and decoding. This opens the door for developers to work with file formats that might've been too large to represent with tables or to write to strings. It also allows for writing algorithms that deal with raw data often, such as compression or hashing. Web services that exchange data in packed formats could also benefit from this. The new type can also serve as a more efficient internal representation for libraries that provide higher level objects like images or geometry data.

Other high level languages support similar data structures, for example [Java ByteByffer](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/nio/ByteBuffer.html) or [JavaScript ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).

## Design

This type will be called 'buffer' and will be implemented using a new built-in type (GCObject with new tag).

By default, metatable is not set for this type and can only be modified using `lua_setmetatable` C API.

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

It is possible for 'source_buffer' and 'target_buffer' to be the same. 
Copying an overlapping region inside the same buffer acts as if the source region is copied into a temporary buffer and then that buffer is copied over to the target.

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

All offsets start at 0 (not to be confused with indices that start at 1 in Luau tables).
This choice is made for both performance reasons (no need to subtract 1) and for compatibility with data formats that often describe field positions using offsets.
While there is a way to solve the performance problem using luajit trick where table array part is allocated from index 0, this would mean that data in the buffer has 1 extra byte and this complicates the bounds checking.

Read and write operations for relevant types are little endian as it is the most common use case, and conversion is often trivial to do manually.

Additionally, unaligned offsets in all operations are valid and behave as expected.

Unless otherwise specified, if a read or write operation would cause an access outside the data in the buffer, an error is thrown.

### Public C API

`void* lua_tobuffer(lua_State* L, int idx, size_t* len);`

Used to fetch buffer data pointer and buffer size at specified location.

If there is no buffer at the location, `NULL` is returned and `len` is not modified.

`void* lua_newbuffer(lua_State* L, size_t l);`

Pushes new buffer of size `l` onto the stack.

`void* luaL_checkbuffer(lua_State* L, int narg, size_t* len);`

Similar to `lua_tobuffer`, but throws a tag error if there is no buffer at specified location.

`int luaopen_buffer(lua_State* L);`

Registers the 'buffer' library. If `luaL_openlibs` is used, that includes the 'buffer' library.

`LUA_BUFFERLIBNAME`

Macro containing the 'buffer' library name.

## Drawbacks

This introduces 'buffer' as a class type in global typing context and adds new global 'buffer' table.
While class type might intersect with user-defined 'buffer' type, such type redefinitions are already allowed in Luau, so this should not cause new type errors.
Same goes for the global table, users can already override globals like 'string', so additional of a new global is backwards-compatible, but new table will not be accessible in such a case.

This increases the complexity of the VM a little bit, since support for new tagged type is required in interpreter loop and GC.

There is also a string buffer C API; by having functions talk about 'buffer' (like `luaL_extendbuffer`) and use `luaL_Buffer`, it might be a point of confusion for C API users.

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
