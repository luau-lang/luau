# bit32.bswap

## Summary

Add `bit32.bswap` to swap the endianness of a 32-bit integer.

## Motivation

The endianness of an integer is generally invisible to Luau users. Numbers are treated as expected regardless of their underlying representation, as is standard across programming languages. However, in some file formats and algorithms, the endianness of an integer is important, so it becomes necessary to swap the order of bytes of an integer to 'pretend' that it is one endian or the other.

While the endianness of numbers can be swapped through a few methods, it is cumbersome. Modern CPUs have instructions dedicated to this (`bswap` on x86-64, `rev` on aarch64) but in Luau, the current best method is to manually shift bytes around and OR them together. For 32-bit integers, this becomes a total of 7 calls:

```lua
bit32.bor(
    bit32.lshift(n, 24),
    bit32.band(bit32.lshift(n, 8), 0xFF0000),
    bit32.band(bit32.rshift(n, 8), 0xFF00),
    bit32.rshift(n, 24),
)
```

Along with being inefficient, it is also difficult read this code and remember it. It took the author of this RFC several tries to write the above example correctly.

## Design

The `bit32` library will gain a new function: `bit32.bswap`:

```
bit32.bswap(n: number): number
```

`bswap` will take the bytes of a number and swap their endianness. To be exact, for an integer `0xA1B2_C3D4`, it will return `0xD4C3_B2A1`.

## Drawbacks

There is a reasonable expectation that `bit32`` functions recieve built-in implementations to improve their performance. This is even more true with native codegen. As this functionality is relatively niche, it may not be worth including it for that reason alone because it would occupy a built-in function slot in the VM.

However even without a built-in call, an initial implementation was still significantly faster than the alternative presented above. So, the only drawback known is in the marginal increase to the overall VM complexity, which is not considered to be a serious drawback.

## Alternatives

A function to simply convert an integer to little-endian was considered, but was rejected due to a basic logic: it is impossible to know whether a given integer is in little-endian so the function may as well be a generic swapping function. Naming such a function is also potentially complex without being verose (`bit32.tole` is a bad name, but `bit32.tolittleendian` is too long).

Simply using the existing `bit32` functions as presented at the beginning of the RFC is not unworkably slow, so it is a viable alternative for a niche use case like this. However, as noted before it is complicated to visually parse.

It may be more reasonable to identify and implement use cases for this function rather than the function itself. However, this is not sustainable: it is doubtful anyone wishes to include support for MD5 hashing natively, as an example.