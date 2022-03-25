# Add primitive function and table types

## Summary

Add types for "real" functions and tables.

## Motivation

Some APIs require "real" functions and tables, not just things that
"look functiony" (e.g. tables with a `__call__` metamethod) or "look
tabley" (e.g. instances of classes). This RFC adds types for those.

## Design

Add:

* a type `table`, inhabited by Luau tables (but not class instances), and
* a type `function`, inhabited by Luau functions (but not class methods or
tables with metamethods).

Luau functions with known source and targe types are now an intersection type `function & (T) -> U`.

Luau tables with known properties are now an intersection type `table & { p : T }`.

We may want to provide syntax sugar `function(T) -> U` and `table{ p : T }` for these, since they will probably be quite common.

We should audit APIs to see which ones accept functiony or tably arguments, and which ones want real functions and tables.

## Drawbacks

Another bit of complexity budget spent.

## Alternatives

Stick with the current imprecision.
