# string.totable

## Summary

Add `string.totable` to convert a `string` to a `table` composed of its underlying bytes. The name is negotiable but unimportant to the nature of the function.

## Motivation

One of the more common approaches to processing strings where the underlying bytes are significant (whether this be for hashing or some other purpose like Base64) is to read the entire contents of the `input` into a `table` that contains those underlying bytes.

This has a rather significant overhead however, so performance sensitive code does not do this and instead works directly with strings using `string.byte`. This complicates logic because it makes it annoying to read from the input (it must be done using `string.byte`) and it makes writing have a rather serious performance implication due to string concatenation.

Given the overhead of turning a `string` into a `table` within Luau, a builtin equivalent seems obvious. Initial testing with a naive implementation suggests a version implemented in C will be at minimum as-fast as pure Luau and significantly faster for some use cases.

## Design

This RFC proposes a function with the following type definition:

```
string.totable(str: string, destination: {number}?, n: number?): {number}
```

This function would take `str`, iterate through it, and place the bytes of the string into a `table` such that `string.byte(str, n, n) == result[n]`. If `destination` is provided, the bytes would be placed into it, either starting at index `1` or iff `n` is provided, starting at index `n`.

If `destination` is provided, it would return it. Otherwise it would return a new table created by the function.

The intention of `destination` and `n` is to allow the function to mesh smoothly with `table.create` (for additional allocation) or reused tables.

## Drawbacks

This function's usefulness is heavily dependent upon usecase. It sees very little performance benefit over a Luau-equivalent code snippet except when the `destination` argument is also utilized. Given this, it may not be worth it.

With the suggested name, it's unclear semantically what to expect from a function named `string.totable`. It is conceivable that Roblox users would assume it to be the equivalent a table composed of every *character* from the `string` rather than its underlying bytes. This can be mitigated with ample documentation and by choosing a new name.

The name is easy to typo, which may lead to annoying bugs. With Luau's excellent analyzer however, this should be less of a problem.

## Alternatives

Alternative names such as `tobytes` and `intotable` were considered but ultimately `totable` was decided upon because it is straightforward. The name is not considered integral to this RFC and may be changed.

Several alternative designs for the function were considered, but they did not mesh well with existing code for varying reasons. To abridge them:

- A design that only accepted the `string` and returned an array of its bytes was considered but was decided against because it makes adding to the array or reusing it inefficient.

- A design that accepted a destination but no offset was considered but felt strange because it precluded the possibility of header data in the table.

- A design that simply accepted a second argument to allocate additional space in the returned table was considerd but rejected because it was bad in a way that is probably obvious.

Generally, the chosen functionality and signature of the function feels complete and in initial testing was effectively a drop-in replacement for several pieces of code.