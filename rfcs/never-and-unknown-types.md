# never and unknown types

## Summary

Add `unknown` and `never` types that are inhabited by everything and nothing respectively.

## Motivation

There are lots of cases in local type inference, semantic subtyping,
and type normalization, where it would be useful to have top and
bottom types. Currently, `any` is filling that role, but it has
special "switch off the type system" superpowers.

Any use of `unknown` must be narrowed by type refinements unless another `unknown` or `any` is expected. For
example a function which can return any value is:

```lua
  function anything() : unknown ... end
```

and can be used as:

```lua
  local x = anything()
  if type(x) == "number" then
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

The `never` type comes up whenever type inference infers incompatible types for a variable, for example

```lua
  function oops(x)
    print("hi " .. x)  -- constrains x must be a string
    print(math.abs(x)) -- constrains x must be a number
  end
```

The most general type of `x` is `string & number`, so this code gives
a type error, but we still need to provide a type for `oops`. With a
`never` type, we can infer the type `oops : (never) -> ()`.

or when exhaustive type casing is achieved:

```lua
  function f(x: string | number)
    if type(x) == "string" then
      -- x : string
    elseif type(x) == "number" then
      -- x : number
    else
      -- x : never
    end
  end
```

or even when the type casing is simply nonsensical:

```lua
  function f(x: string | number)
    if type(x) == "string" and type(x) == "number" then
      -- x : string & number which is never
    end
  end
```

The `never` type is also useful in cases such as tagged unions where
some of the cases are impossible. For example:

```lua
  type Result<T, E> = { err: false, val: T } | { err: true, err: E }
```

For code which we know is successful, we would like to be able to
indicate that the error case is impossible. With a `never` type, we
can do this with `Result<T, never>`. Similarly, code which cannot succeed
has type `Result<never, E>`.

These types can _almost_ be defined in current Luau, but only quite verbosely:

```lua
  type never = number & string
  type unknown = nil | number | boolean | string | {} | (...never) -> (...unknown)
```

But even for `unknown` it is impossible to include every single data types, e.g. every root class.

Providing `never` and `unknown` as built-in types makes the code for
type inference simpler, for example we have a way to present a union
type with no options (as `never`). Otherwise we have to contend with ad hoc
corner cases.

## Design

Add:

* a type `never`, inhabited by nothing, and
* a type `unknown`, inhabited by everything.

And under success types (nonstrict mode), `unknown` is exactly equivalent to `any` because `unknown`
encompasses everything as does `any`.

The interesting thing is that `() -> (never, string)` is equivalent to `() -> never` because all
values in a pack must be inhabitable in order for the pack itself to also be inhabitable. In fact,
the type `() -> never` is not completely accurate, it should be `() -> (never, ...never)` to avoid
cascading type errors. Ditto for when an expression list `f(), g()` where the resulting type pack is
`(never, string, number)` is still the same as `(never, ...never)`.

```lua
  function f(): never error() end
  function g(): string return "" end

  -- no cascading type error where count mismatches, because the expression list f(), g()
  -- was made to return (never, ...never) due to the presence of a never type in the pack
  local x, y, z = f(), g()
  -- x : never
  -- y : never
  -- z : never
```

## Drawbacks

Another bit of complexity budget spent.

These types will be visible to creators, so yay bikeshedding!

Replacing `any` with `unknown` is a breaking change: code in strict mode may now produce errors.

## Alternatives

Stick with the current use of `any` for these cases.

Make `never` and `unknown` type aliases rather than built-ins.
