# bit32.countlz/countrz

**Status**: Implemented

## Summary

Add bit32.countlz (count left zeroes) and bit32.countrz (count right zeroes) to accelerate bit scanning

## Motivation

All CPUs have instructions to determine the position of first/last set bit in an integer. These instructions have a variety of uses, the popular ones being:

- Fast implementation of integer logarithm (essentially allowing to compute `floor(log2(value))` quickly)
- Scanning set bits in an integer, which allows efficient traversal of compact representation of bitmaps
- Allocating bits out of a bitmap quickly

Today it's possible to approximate `countlz` using `floor` and `log` but this approximation is relatively slow; approximating `countrz` is difficult without iterating through each bit.

## Design

`bit32` library will gain two new functions, `countlz` and `countrz`:

```
function bit32.countlz(n: number): number
function bit32.countrz(n: number): number
```

`countlz` takes an integer number (converting the input number to a 32-bit unsigned integer as all other `bit32` functions do), and returns the number of consecutive left-most zero bits - that is, the number of most significant zero bits in a 32-bit number until the first 1. The result is in `[0, 32]` range.

For example, when the input number is `0`, it's `32`. When the input number is `2^k`, the result is `31-k`.

`countrz` takes an integer number (converting the input number to a 32-bit unsigned integer as all other `bit32` functions do), and returns the number of consecutive right-most zero bits - that is,
the number of least significant zero bits in a 32-bit number until the first 1. The result is in `[0, 32]` range.

For example, when the input number is `0`, it's `32`. When the input number is `2^k`, the result is `k`.

> Non-normative: a proof of concept implementation shows that a polyfill for `countlz` takes ~34 ns per loop iteration when computing `countlz` for an increasing number sequence, whereas
> a builtin implementation takes ~4 ns.

## Drawbacks

None known.

## Alternatives

These functions can be alternatively specified as "find the position of the most/least significant bit set" (e.g. "ffs"/"fls" for "find first set"/"find last set"). This formulation
can be more immediately useful since the bit position is usually more important than the number of bits. However, the bit position is undefined when the input number is zero,
returning a sentinel such as -1 seems non-idiomatic, and returning `nil` seems awkward for calling code. Counting functions don't have this problem.

An early version of this proposal suggested `clz`/`ctz` (leading/trailing) as names; however, using a full verb is more consistent with other operations like shift/rotate, and left/right may be easier to understand intuitively compared to leading/trailing. left/right are used by C++20.

Of the two functions, `countlz` is vastly more useful than `countrz`; we could implement just `countlz`, but having both is nice for symmetry.
