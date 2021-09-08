# Unsealed table assignment creates an indexer

In Luau, tables have a state, which can, among others, be "unsealed".
An unsealed table is one that we are still constructing.  Currently
assigning a table literal to an unsealed table creates new properties,
but this is a problem in cases where tables are being used as hash
tables.  We would like to change this so that assigning a table
literal to an unsealed table creates an indexer rather than
properties.

## Motivation

In lua-apps, there is testing code which (simplified) looks like:

```lua
local t = { u = {} }
t = { u = { p = 37 } }
t = { u = { q = 5 } }
local x: number? = t.u["p"]
local y: number? = t.u["q"]
```

Currently, this code doesn't typecheck, due to `p` and `q` being unknown properties of `t`.

In order to support this idiom, we propose that assigning a table with
properties of type `T` to to an unsealed table with no properties
should introduce a string indexer of type `T`.

For example, with this change the type of `t` is `{ u: { [string]: number } }`.

## Drawbacks

Currently table literals are sealed tables, so support width subtyping, which causes
the system to be unsound, for example:

```lua
local t = { u = {} }
local u : { p = 37 } = { p = 37, q = "hi" }
t = { u = u }
local x: number? = t.u["p"]
local y: number? = t.u["q"] -- This is "hi" at run-time
```

The fix for this would be for table literals to be unsealed tables, and to only introduce an indexer when the subtype is unsealed.

This fix needs a change to lua-apps, since sometimes the code uses
assignment with a table literal to add properties, and sometimes to
initialize a hash table. The code currently in lua-apps looks like:

```lua
local t = { } -- Note: no u property
t = { u = { p = 37 } }
t = { u = { q = 5 } }
local x: number? = t.u["p"]
local y: number? = t.u["q"]
```

## Alternatives

Rather than introducing an indexer, we could introduce optional properties. For example we could infer the type of 
`t` as `{ u: { p: number?, q: number? } }`.