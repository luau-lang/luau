# table.clear

> Note: this RFC was adapted from an internal proposal that predates RFC process and as such doesn't follow the template precisely

**Status**: Implemented

## Summary

Add `table.clear` function that removes all elements from the table but keeps internal capacity allocated.

## Design

`table.clear` adds a fast way to clear a Lua table. This is effectively a sister function to `table.create()`, only for reclaiming an existing table's memory rather than pre-allocating a new one. Use cases:

* Often you want to recalculate a set or map data structure based on a table. Currently there is no good way to do this, the fastest way is simply to throw away the old table and construct a new empty one to work with. This is wasteful since often the new structure will take a similar amount of memory to the old one.

* Sometimes you have a shared table which multiple scripts access. In order to clear this kind of table, you have no other option than to use a slow for loop setting each index to nil.

These use cases can technically be accomplished via `table.move` moving from an empty table to the table which is to be edited, but I feel that they are frequent enough to warrant a clearer more understandable method which has an opportunity to be more efficient.

Like `table.move`, does not invoke any metamethods. Not that it would anyways, given that assigning nil to an index never invokes a metamethod.
