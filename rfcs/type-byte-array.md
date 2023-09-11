# Byte Array Type

## Summary

A new built in type to serve as an array of bytes, with a library for reading and writing to the internal buffer. A particularly good example type that this could be derived from is [this Java class](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/nio/ByteBuffer.html).

## Motivation

With this type, we ideally solve the use cases for binary format encoding and decoding, compression, and active/compact memory. This opens the door for developers to work with file formats that might've been too large to represent with tables or to write to strings. It also allows for writing algorithms that deal with raw data often, such as compression or hashing. Web services that exchange data in packed formats could also benefit from this.

## Design

This could ideally be exposed as a new Luau library similar to that of `string` or `table`. It would contain functions for:

* Instantiating a new byte array object with an initial size.

* Resizing an existing object while keeping previously assigned values.

* Reading...

    * ... signed and unsigned integers of 8, 16, and 32 bits given an offset and endianness.

    * ... floating point values of 32 and 64 bits given an offset and endianness.

    * ... a string given an offset and size.

 * Writing...

    * ... signed and unsigned integers of 8, 16, and 32 bits given an offset, value, and endianness.

    * ... floating point values of 32 and 64 bits given an offset, value, and endianness.

    * ... a string given an offset, size, and value.

As Luau can't represent 64 bit integers in user code, the functions for these could be omitted or simply serve as a shortcut for writing what can be represented already without needing 2 calls to the 32 bit functions.

## Drawbacks

Depending on implementation this could increase the complexity of the VM and related code. If this is to be implemented as a built-in, optimized type, it might need specialized fast paths for all relevant opcodes. Additionally, this type would have to come in with a whole new library table as part of the global environment, which could cause name collisions in older code.

## Alternatives

The workarounds without this feature are significantly inefficient:

* Tables can, at most, represent 64 bits per slot using expensive `vector` packing.

* Tables with or without packing severely bloat memory, as each array entry is subject to Luau value size and alignment.

* Strings are immutable and can’t be used to efficiently construct binary data without exponential allocations.

* Built in `string.pack` and `string.unpack` can’t cover more complex schemas on their own or formats which are edited mid-creation.
