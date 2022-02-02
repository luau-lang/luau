# Generalized iteration

## Summary

Introduce support for iterating over tables without using `pairs`/`ipairs` as well as a generic customization point for iteration via `__iter` metamethod.

## Motivation

Today there are many different ways to iterate through various containers that are syntactically incompatible.

To iterate over arrays, you need to use `ipairs`: `for i, v in ipairs(t) do`. The traversal goes over a sequence `1..k` of numeric keys until `t[k] == nil`, preserving order.

To iterate over dictionaries, you need to use `pairs`: `for k, v in pairs(t) do`. The traversal goes over all keys, numeric and otherwise, but doesn't guarantee an order; when iterating over arrays this may happen to work but is not guaranteed to work, as it depends on how keys are distributed between array and hash portion.

To iterate over custom objects, whether they are represented as tables (user-specified) or userdata (host-specified), you need to expose special iteration methods, for example `for k, v in obj:Iterator() do`.

All of these rely on the standard Lua iteration protocol, but it's impossible to trigger them in a generic fashion. Additionally, you *must* use one of `pairs`/`ipairs`/`next` to iterate over tables, which is easy to forget - a naive `for k, v in tab do` doesn't work and produces a hard-to-understand error `attempt to call a table value`.

This proposal solves all of these by providing a way to implement uniform iteration with self-iterating objects by allowing to iterate over objects and tables directly via convenient `for k, v in obj do` syntax, and specifies the default iteration behavior for tables, thus mostly rendering `pairs`/`ipairs` obsolete - making Luau easier to use and teach.

## Design

In Lua, `for vars in iter do` has the following semantics (otherwise known as the iteration protocol): `iter` is expanded into three variables, `gen`, `state` and `index` (using `nil` if `iter` evaluates to fewer than 3 results); after this the loop is converted to the following pseudocode:

```
while true do
  vars... = gen(state, index)
  index = vars... -- copy the first variable into the index
  if index == nil then break end
  
  -- loop body goes here
end
```

This is a general mechanism that can support iteration through many containers, especially if `gen` is allowed to mutate state. Importantly, the *first* returned variable (which is exposed to the user) is used to continue the process on the next iteration - this can be limiting because it may require `gen` or `state` to carry extra internal iteration data for efficiency. To work around this for table iteration to avoid repeated calls to `next`, Luau compiler produces a special instruction sequence that recognizes `pairs`/`ipairs` iterators and stores the iteration index separately.

Thus, today the loop `for k, v in tab do` effectively executes `k, v = tab()` on the first iteration, which is why it yields `attempt to call a table value`. If the object defines `__call` metamethod then it can act as a self-iterating method, but this is not idiomatic, not efficient and not pure/clean.

This proposal comes in two pars: general support for `__iter` metamethod and default implementation for tables without one.

## Drawbacks

Why should we *not* do this?

## Alternatives

What other designs have been considered? What is the impact of not doing this?
