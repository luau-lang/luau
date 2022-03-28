# Add never and unknown types

## Summary

Add `unknown` and `never` types that are inhabited by everything and nothing respectively.

## Motivation

There are lots of cases in local type inference, semantic subtyping,
and type normalization, where it would be useful to have top and
bottom types. Currently, `any` is filling that role, but it has
special "switch off the type system" superpowers.

The `never` type comes up whenever type inference infers incompatible types for a variable, for example

```lua
  function oops(x)
    print("hi " ++ x)  -- constrains x must be a string
    print(math.abs(x)) -- constrains x must be a number
  end
```

The most general type of `x` is `string & number`, so this code gives
a type error, but we still need to provide a type for `oops`. With a
`never` type, we can infer the type `oops : (never) -> ()`.

The `never` type is also useful in cases such as tagged unions where
some of the cases are impossible. For example:

```lua
  type Result<T, E> = { err: false, val: T } | { err: true, err: E }
```
For code which we know is successful, we would like to be able to
indicate that the error case is impossible. With a `never` type, we
can do this with `Result<T, never>`. Similarly, code which cannot succeed
has type `Result<never, E>`.

The `unknown` case is useful in cases where the user of an API must
use type casing to use the API safely. For example a function which
can return any value is:

```lua
  function anything() : unknown ... end
```

and can be used as:

```lua
  local x = anything()
  if type(x) == "number"
    print(x + 1)
  end
```

The type of this function cannot be given concisely in current
Luau. The nearest equivalent is `any`, but this switches off the type system, for example
if the type of `anything` is `() -> any` then the following code typechecks:

```lua
  local x = anything()
  print(x + 1)
```

This is fine in nonstrict mode, but strict mode should flag this as an error.

These types can be defined in current Luau, but only quite verbosely:

```lua
  type never = number & string
  type unknown = nil | number | boolean | string | {} | Instance | (never...)->(unknown...)
```

Providing `never` and `unknown` as built-in types makes the code for
type inference simpler, for example we have a way to present a union
type with no options (as `never`). Otherwise we have to contend with ad hoc
corner cases.

## Design

Add:

* a type `never`, inhabited by nothing, and
* a type `unknown`, inhabited everything.

Use them, rather than `any` where appropriate. Ideally, we would then never infer `any`.

## Drawbacks

Another bit of complexity budget spent.

These types will be visible to creators, so yay bikeshedding!

Replacing `any` with `unknown` is a breaking change: code in strict mode may now produce errors.

## Alternatives

Stick with the current use of `any` for these cases.

Make `never` and `unknown` type aliases rather than built-ins.