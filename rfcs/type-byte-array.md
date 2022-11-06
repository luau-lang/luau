# Byte Array Type

## Summary

A new constructed type which serves as a mutable array of bytes. Ideally, it would expose an API that would allow for building, reading, and writing to the internal buffer. A particularly good example type that this could be derived from is [this Java class](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/nio/ByteBuffer.html).

## Motivation

With this type, we ideally solve the use cases for binary format encoding and decoding, compression, and active/compact memory. This opens the door for developers to work with file formats that might've been too large to represent with tables or to write to strings. It also allows for writing algorithms that deal with raw data often, such as compression or hashing. Web services that exchange data in packed formats could also benefit from this.

## Design

While an API as extensive as the example might be good, it's only really necessary to have methods for treating this block of data as a random access collections of bytes with potentially resizable backing. This could ideally be exposed as:

* A constructor with predefined size and endianness.

* A method for resizing the existing object while retaining data in bounds.

* Methods for reading...

    * ... a string given an offset and size.

    * ... signed and unsigned integers of 8, 16, and 32 bits given an offset.

    * ... floating point values of 32 and 64 bits given an offset.

 * Methods for writing...

    * ... a string given an offset, size, and value.

    * ... signed and unsigned integers of 8, 16, and 32 bits given an offset and value.

    * ... floating point values of 32 and 64 bits given an offset and value.

As Luau can't represent 64 bit integers in user code, the methods for these could be omitted or simply serve as a shortcut for writing what can be represented already without needing 2 calls to the 32 bit methods.

## Drawbacks

Depending on implementation this could increase the complexity of the VM and related code. If this is to be implemented as a built-in, optimized type it might need specialized fast paths for all relevant opcodes. Additionally, this type would have to come in with methods and some sort of constructor to be useful, which does not have precedent in the open source Luau distribution (even `vector` lacks an exposed constructor or methods).

## Alternatives

The workarounds without this feature are significantly inefficient:

* Tables can, at most, represent 64 bits per slot using expensive `vector` packing.

* Tables with or without packing severely bloat memory, as each array entry is subject to Luau value size and alignment.

* Strings are immutable and can’t be used to efficiently construct binary data without exponential allocations.

* Built in `string.pack` and `string.unpack` can’t cover more complex schemas on their own or formats which are edited mid-creation.
