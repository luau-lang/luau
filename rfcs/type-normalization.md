# Type normalizaton

## Summary

We should normalize types, for example removing redundant unions and
intersections, to minimize memory usage and make user-visible inferred
types easier to read.

## Motivation

Once local type inference lands, we will infer union types more
often. This is good, in that we will be inferring more precise types,
but does mean we have to consider the impact this has on memory, and
the complexity of the user-visible types.

Consider the program:
```
  function f()
    local x:T = ...
    local y:U = ...
    if g() then return x else return y end
  end
```

Currently, this program produces a type error, but with local type inference
it does not, and instead introduces two subtyping constraints on the free return type:
```
  T <: R    U <: R
```

which is solved as
```
  R = T | U
```

This can produce unexpected types, for example `number|number`
or `Animal|Cat`. Conversely, a program like

```
 local x:T = ...
 local y:U = ...
 local z = ...
 x = z
 y = z
```

will introduce a new free type `Z` for `z`, with constraints 
```
  Z <: T    Z <: U
```

which is solved as
```
  Z = T & U
```

This can also produce unexpected types, for example `number&number`
or `Animal&Cat`.

## Design

In this section, we outline some possible designs. These fall into two
broad camps: syntactic subtyping uses rewrite rules on the syntax of
types, whereas semantic subtyping uses semantic models of types.

### Syntactic subtyping

#### Alternative: check for pointer equality before adding to a union/intersection

To normalize a union or intersection, iterate through the options, adding them to a
vector. Don't add them if the type is already present (a cheap check
for pointer equality).

Advantages: easy to implement; fast.

Disadvantage: pointer equality is a very brittle test; it is easy to still get
types `T|T`, caused by having two clones of `T`.

This is currently what we do.

#### Alternative: check for suptyping before adding to a union/intersection

Ditto, but don't add a type if there's already a supertype in the
vector (subtype in the case of intersections).

Advantages: easy to implement (reuses the existing subtyping
machinery). Does most cases we're interesred in (e.g. `number|number`
or `Cat|Animal`).

Disadvantages: doesn't deal with mixed intersections and unions
(e.g. `Animal&Cat` doesn't simplify); depends on the order of types
(e.g. `Cat&Animal` normalizes to `Cat`).

#### Alternative: ditto but convert to disjunctive normal form first

Use the fact that intersection distributes through union
```
  (T | U) & V  ==  (T & V) | (U & V)
```

to move union out of intersection before normalizing.

Advantage: does a better job of some examples with tables:

```
   ({ p : T } | { q : U }) & { r : V }
     == ({ p : T } & { r : V }) | ({ q : U } & { r : V })
     == ({ p : T, r : V }) | ({ q : U, r : V })
```

Disadvantage: exponential blowup.

#### Alternative: normalize union and intersection of tables to tables

We can work up to the equivalence used in the last example, normalizing `{ p : T } & { q : U }` as `{ p : T, q : U }`.

For read-write properties `{ p : T } & { p : U }` is inhabited only when `T == U`, otherwise it is an uninhabited type (so equivalent to `{ p : never }`).

For read-only properties `{ get p : T } & { get p : U }` is inhabited by the same values as `{ get p : T&U }`. (Ditto union).

Advantage: a better presentation of intersections of tables.

Disadvantage: developers might be surprised that `{ p : number } & { p : number? }` is `{ p : never }`; recursive unions and intersections increase time complexity; there is less normalization of unions-of-tables.

#### Alternative: normalize union and intersection of functions to functions

Similarly, we can treat `(T -> U) | (T -> V)` as `T -> (U | V)` and `(T -> V) | (U -> V)` as `(T & U) -> V`.

There is no good normalization of `(T -> U) | (V -> W)` in general. The obvious candidate is `(T & V) -> (U | W)` but there are functions of that type that are not of type `T -> U` or `V -> W`, such as `function(x) if random() then U.new() else W.new() end`.

Advantage: normalizes some functions.

Disadvantage: doesn't do what you expect. More recursive normalization. Has nasty interactions with overloaded functions. Requires union and intersection of type packs.

## Semantic subtyping

The idea here is to think of types as sets of values, where intersection and union have their usual interpretation on sets.

Values can be thought of as trees, for example a table has children given by the properties, so types are sets of trees such as
```
   table     table
    / \       / \
{  p   q  ,  p   q  , ... }
   ↓   ↓     ↓   ↓
 true  1  false  5
```

is the set of trees corresponding to `{ p : boolean , q : number }`.

In semantic subtyping, the subtyping replation is interpreted as subset order.

To make subset inclusion tractable, we can make use of finite state automata.

In the same way that sets of strings are the languages of automata, sets
of trees are the languages of *tree automata*. Many of the same
techniques, such as minimization, construction of union and
intersection, etc, apply to tree automata. The main difference is that
nondeterministic top-down tree automata are strictly more powerful than derministic ones.

Tree automata have:

* a set of states `Q`,
* an initial state `q0`, and
* transitions of the form `q0 -> f(p1 = q1, ..., pN = qN)` (where `f` is a function symbol, `pI` are distinct property names, and `qI` are states).

(Luau has namned properties rather than positional arguments, but otherwise this is standard).

An automaton accepts a tree if its initial state does. A state `q` accepts a tree `t`
if there is a transition `q0 -> f(p1 = q1, ..., pN = qN)` where `t` has node label `f`,
and children `t -p1-> t1`,..., `t -pN-> tN` where each `qI` accepts `tI`.

Draw automata where the transition `q0 -> f(p1 = q1, ..., pN = qN)` as
```
         ◇
         |
         f
         ↓
      .--□--.
      |     |
     p1 ... pN
      ↓     ↓
      ◇     ◇
```

For example, the tree automaton for `{ p: boolean, q: number }` is:
```
         ◇
         |
       table
         ↓
      .--□--.
     /       \
    p         q
    ↓         ↓
    ◇         ◇
   / \        |
true false    n
  ↓   ↓       ↓
  □   □       □
```

A relation `R` on states is a simulation whenever

 * if `(q, r) ∈ R` and `q -> f(p1=q1, ..., pM=qM)'` then `r -> f(p1=r1,...,pN=rN)'` and for every `qI`, `(qI, rI) ∈ R`.

A relation is a bisimulation if both it and its inverse are simulations.

[Partition refinement](https://en.wikipedia.org/wiki/Partition_refinement)
computes bisimulation on NFAs, which coincides with language
equivalence on DFAs. It can be adapted to compute simulation. We can
use it to compute a partition of types in the type graph, up to
semantic equivalence of types.

Once we have a partition, there is a question of which representative
to pick.  We can read off the minimal type from the partitioned type
graph, but this has lost any names given to types by type aliases.
We could use any named type from the partition if there is one, and otherwise
synthesize a minimal type.

Related work:

* XDuce: original work on types-as-tree-automata
* CDuce: home of semantic subtyping
* XML Schema, XPath, JSON Schema...: schema and query languages based on tree automata.

Advantages: Computes minimal types; has a sound foundation.

Disadvantages: complex; the size benefits of minimal types may be overwhelmed by the time to run partitioning.

## Drawbacks

The cost of computing minimal types may be too much.

## Alternatives

This is a draft RFC outlining the alternatives.
