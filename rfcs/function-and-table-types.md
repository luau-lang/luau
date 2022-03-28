# Add primitive function and table types

## Summary

Add types for "real" functions and tables.

## Motivation

Some APIs require "real" functions and tables, not just things that
"look functiony" (e.g. tables with a `__call` metamethod) or "look
tably" (e.g. instances of classes). This RFC adds types for those.

For example, the function:

```lua
  function succ(x)
    if type(x) == "function"
      return x() + 1
    else
      assert(type(x) == number)
      return x + 1
    end
  end
```

cannot quite be given an accurate Luau type. The nearest is `(number |
()->number) -> number` but this is slightly too generous, since it
allows "functiony" types such as tables with a `__call` metamethod:

```lua
  local t = setmetatable({}, {__call = function(self) return 5 end})
  succ(t)
```

This will typecheck but produce a runtime error.

A similar issue affects tables.

The common cases in practice are built-in APIs such as the Luau
standard library, or APIs implemented using the C++ FFI. For example,
`pairs` takes a table argument, and gives a runtime error if it is called
with a class instance.

## Design

Add:

* a type `table`, inhabited by Luau tables (but not class instances), and
* a type `function`, inhabited by Luau functions (but not class methods or
tables with metamethods).

Luau functions with known source and target types are now an intersection type `function & (T) -> U`.

Luau tables with known shape are now an intersection type `table & { p : T }`.

We may want to provide syntax sugar `function(T) -> U` and `table{ p : T }` for these, since they will probably be quite common.

We should audit APIs to see which ones accept functiony or tably arguments, and which ones want real functions and tables.

The use of intersecton types `table & { p: T }` depends on the [Shape types RFC](shape-types).

## Drawbacks

Another bit of complexity budget spent.

## Alternatives

Stick with the current imprecision.
