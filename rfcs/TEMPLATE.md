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



## Drawbacks

Why should we *not* do this?

## Alternatives

What other designs have been considered? What is the impact of not doing this?
