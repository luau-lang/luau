# Type normalizaton

## Summary

Normalize types, for example removing redundant unions and
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

In this section, we outline a number of possible designs. These fall into two broad camps: syntactic subtyping uses rewrite rules on the syntax of types, whereas semantic subtyping uses semantic models of types.

### Syntactic subtyping

#### Alternative: check for pointer equality before adding to a union/intersection

To normalize a union or intersection, iterate through the options, adding them to a
vector. Don't add them if the type is already present (a cheap check
for pointer equality).

Advantages: easy to implement; fast.

Disadvantage: pointer equality is a very brittle test; it is easy to still get
types `T|T`, caused by having two clones of `T`.

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
     o         o
    / \       / \
{  p   q  ,  p   q  , ... }
   ↓   ↓     ↓   ↓
 true  1  false  5
```

is the set of trees corresponding to `{ p : boolean , q : number }`.

In semantic subtyping, the subtyping replation is interpreted as subset order.

In the same way that sets of strings are the languages of automata, sets
of trees are the languages of *tree automata*. Many of the same
techniques, such as minimization, construction of union and
intersection, etc, apply to tree automata. The main difference is that
nondeterministic top-down tree automata are strictly more powerful than derministic ones.

Tree automata are given by rules of the form `q0 -> f(q1, ..., qN)` where

 * each `qI` is a state of the automaton, and
 * `f` is a symbol used to label a tree node with `N` children.

for example, the automaton for `{ p : boolean, q : number }` has initial state `q0` and transitions:
```
  q0 -> { p = q1, q = q2 }
  q1 -> true
  q1 -> false
  q2 -> n for any number n
```

Tree automata are closed under union and intersection, in the same way that string automata are.

## Drawbacks

Why should we *not* do this?

## Alternatives

What other designs have been considered? What is the impact of not doing this?
