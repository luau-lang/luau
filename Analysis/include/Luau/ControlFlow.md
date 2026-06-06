This file just summarizes some design decisions I am making/made for the Control Flow Graph for other folks who will work on this + for myself.

## High level goal
- Make refinements and type stating more reliable (hopefully will bring down the devforum bug report rate)
- Make type stating aware of back edges in control flow.
- Make the implementation of these in the new solver easier to reason about.
- Open up new avenues for analysis (e.g effect tracking, generalizing scc's of mutually recursive functions etc)

## Solution
Introduce a Control Flow Graph construct for Luau to replace the current Data Flow Graph based on [this paper](https://bernsteinbear.com/assets/img/braun13cc.pdf)

## Decisions made
This CFG is in SSA form, and allows you to answer questions like: 'which version of this variable am I referring to'.

### Definition
A `Definition` is a pointer that describes a versioned access to a variable. It is represented as a (AstLocal*, size_t) pair.
In the following code:
```
local x = 0
local y = 0
local z = 5
x = y + x
y = x + z
```
we could rewrite this in a versioned form like:
```
local x_0 ...
local y_0 ...
local z_0 ...
x_1 = y_0 + x_0
y_0 = x_1 + z+0
```

The CFG maintains a mapping, so that in rhs positions, you can ask questions like what is the def for the `AstExprLocal` associated with `y` on line 4 of the original code.

### Joins
A `Join` represent a use of a variable that depends on > 1 definition of a variable in a predecessor block.

```
local x = 0
if true then
    x = nil
end
use(x)
```
might be represented as something like:
```
local x_0 ...
if true then
    x_1 = nil
end
x_2 = join(x_0, x_1)
use(x_2)
```

## Conditions
Conditions in control flow have a number of interesting properties.
1) Sequences of conditions generate refinements that are only visible in the scope of the refinement.
For example: `a and a.|{b |c } and a.b.|{c | d | e}` generates a sequence of refinements that are visible
to `a.` and `a.b`, but might not be visible outside.
2) Type refinements are a way of generating predicates on types in select scopes
3) Type refinements go out of scope!


## Globals
I've already tweaked the representation of a Def to store a Symbol, which is basically like AstLocal* | Global.

## Indexing
We should augment the definition of a Definition to something like:
```
DefPtr = Variant<Definition, Indexed>
where Definition = (Symbol, version)
and IndexedDefinition = (Definition, <path>)
```
