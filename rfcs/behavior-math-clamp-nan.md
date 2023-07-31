# `math.clamp(nan, a, b)` returns a instead of `nan`

## Summary

`math.clamp(nan, a, b)` will return a instead of `nan`.

## Motivation

`math.clamp` is documented to clamp the number to the provided range, but does not guarantee behavior for NaN inputs:

> Returns n if the number is in [min, max] range; otherwise, returns min when n < min, and max otherwise. If n is NaN, may or may not return NaN.

In practice, `math.clamp` is implemented to preserve NaN. This means that `math.clamp` will return a number that is inside the provided range, or NaN.
This is inconvenient because the special case may not be expected, and the resulting number may be an input to an algorithm that does not tolerate NaNs.

This can be a problem if:

- The input value is a result of a complex computation. Even if the code is tested on a wide range of numbers, NaN may be forgotten.
- The input value is a user input; when `tonumber` is used to parse it, NaN is one of the possible outcomes but it may not be handled appropriately
- The input value is a remote event/function argument. `math.clamp` can be used to clamp it to a valid range, but due to a network boundary a malicious client may inject NaN that will not be clamped.

## Design

`math.clamp(t, a, b)` will result in an error if `a <= b` does not hold, and will otherwise return:

- t, if a <= t <= b
- a, if t < a
- b, if t > b
- a, if t is NaN

As a consequence of this change, `math.clamp` return value `r` will always satisfy `a <= r <= b`.
Note that if the input or one of the limits is a negative zero we will not guarantee that the sign of the output is preserved; positive and negative zeros are considered equal under this proposal.

The behavior of NaN clamping outside of Luau varies. Shading languages (HLSL, GLSL in Vulkan, MSL in Metal) typically guarantee that `clamp(t, a, b)` is equal to a when t is NaN; C++ `std::clamp` provides no guarantees and in fact treats NaN as a technically-invalid input; Rust preserves NaN; Zig returns the upper limit. Swift does not implement clamp for floats. Despite the differences, we believe that for Luau domain guaranteeing that the result is in the specified range is more valuable than preserving NaN, and returning the lower limit is more consistent with industry practice than returning upper limit.

> The following text is non-normative as it relates to the specific implementation consequences.

Currently, `clamp(t, a, b)` is defined as `min(b, max(a, t))`, where `min(a, b) = a < b ? a : b` and `max(a, b) = a > b ? a : b`. Note that these definitions of `min/max` are different from `math.min`/`math.max` but happen to coincide with behavior of `MINSD/MAXSD` instructions on Intel architecture.
Under IEEE-754 rules, this leads to `max(a, t)` and `min(b, t)` preserving t when it is NaN. This proposal would require defining `clamp(t, a, b)` as `min(b, max(t, a))` (swapping the order of arguments to `max`).

In addition to changing the behavior for NaN, this will also have a side-effect of clamping negative zero differently: `math.clamp(-0, 0, 1)` is currently -0 but it will return 0 after the suggested changes. The effects of this should be benign.

This change by itself is expected to be neutral on performance, as the codegen should be virtually unchanged on any architecture.
A sufficiently smart C compiler[^1] or a sufficiently motivated codegen implementation may result in performance improvement after this change on ARM: the above definitions of min/max use two instructions on A64, however after the proposed change an alternative implementation of clamp is possible: `clamp(t, a, b) = fmin(b, fmax(t, a))`, where `fmin`/`fmax` use C99 semantics and can be lowered to `FMINNM/FMAXNM`. This is possible because `a` and `b` are guaranteed to be finite after the error check.

## Drawbacks

This technically changes behavior; it is possible that some code is relying on `math.clamp` preserving NaN and is checking the result for NaN instead of the input. This is unlikely, and code like this can easily be adjusted.

If NaN should be impossible as an input, the current behavior may help catch bugs like this in development, assuming that the algorithm that uses the result of `math.clamp` breaks in a somewhat visible way. Returning an in-range value may hide the error.

## Alternatives

We could decide that `math.clamp` errors when the input is NaN, but that results in strictly worse behavior, as now unsanitized input may completely break unsuspecting code.

We could decide that `math.clamp` returns the upper bound to match Zig but it does not seem to be better than matching shading languages.

[^1]: No existing production C compiler is currently sufficiently smart.
