# Change `assert` to return one value

> Note: this RFC was adapted from an internal proposal that predates RFC process

## Summary

Change `assert` to only return the first argument on success

## Motivation

Today `assert()` function has confusing semantics:

`assert(x)` fails if `x` is falsy and returns `x` otherwise (this is okay!)
`assert(x, y)` fails with `y` as the error message if `x` is falsy, and returns `x, y` otherwise (why?)
`assert(x, y, z)` fails with `y` as the error message if `x` is falsy, and returns `x, y, z` otherwise (why???)

It's not clear what purpose is there behind returning more than one argument from `assert`, as it doesn't seem like it can be useful.

Specifically, when a two-argument form is used, the second argument must be an error message (otherwise if the first argument is falsy, the error behavior will be confusing); if it *is* in fact an error message, it's not clear why it's useful to return that error message when the first argument was truthful.

When a three-argument form is used, the third argument is just ignored and passed through, again for no apparent benefit.

## Design

This proposal argues that long term it's cleaner for us to fix the semantics here to just return the first argument. Any extra arguments will be ignored at runtime as usual, but our type checker can start treating these as invalid.

To be more precise, we'd be formally switching from

```
declare function assert<T...>(...: T...): T...
```

to

```
declare function assert<T>(value: T, error: string?): T
```

This allows us to align the runtime behavior of `assert` to the type definition and reduces the chance of an accidental error - since all return values are forwarded when the call is used as the last function argument, this code transformation is safe:

```
- foo(bar)
+ foo(assert(bar))
```

But this code transformation may be unsafe if `foo` has optional parameters:

```
- foo(assert(bar))
+ foo(assert(bar, "bar should not be nil"))
```

After this proposal the transformation becomes safe.

## Drawbacks

This may break user code. Unfortunately there is only one way for us to find out if it does, which is to roll this change out. We will use release notes to communicate the change and, if concerns are raised by the community ahead of time, we will investigate them.

## Alternatives

Alternatively we could keep `assert` behavior exactly as it is; this would match mainline Lua but we want to make the language better and if we were designing the standard library again we'd never make this choice, hence the proposal.
