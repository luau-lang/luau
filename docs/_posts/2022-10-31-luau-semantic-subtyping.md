---
layout: single
title:  "Semantic Subtyping in Luau"
author: Alan Jeffrey
---

Luau is the first programming language to put the power of semantic subtyping in the hands of millions of creators.

## Minimizing false positives

One of the issues with type error reporting in tools like the Script Analysis widget in Roblox Studio is *false positives*. These are warnings that are artifacts of the analysis, and don’t correspond to errors which can occur at runtime. For example, the program
```lua
  local x = CFrame.new()
  local y
  if (math.random()) then
    y = CFrame.new()
  else
    y = Vector3.new()
  end
  local z = x * y
```
reports a type error which cannot happen at runtime, since `CFrame` supports multiplication by both `Vector3` and `CFrame`. (Its type is `((CFrame, CFrame) -> CFrame) & ((CFrame, Vector3) -> Vector3)`.)

False positives are especially poor for onboarding new users. If a type-curious creator switches on typechecking and is immediately faced with a wall of spurious red squiggles, there is a strong incentive to immediately switch it off again.

Inaccuracies in type errors are inevitable, since it is impossible to decide ahead of time whether a runtime error will be triggered. Type system designers have to choose whether to live with false positives or false negatives. In Luau this is determined by the mode: `strict` mode errs on the side of false positives, and `nonstrict` mode errs on the side of false negatives.

While inaccuracies are inevitable, we try to remove them whenever possible, since they result in spurious errors, and imprecision in type-driven tooling like autocomplete or API documentation.

## Subtyping as a source of false positives

One of the sources of false positives in Luau (and many other similar languages like TypeScript or Flow) is *subtyping*. Subtyping is used whenever a variable is initialized or assigned to, and whenever a function is called: the type system checks that the type of the expression is a subtype of the type of the variable. For example, if we add types to the above program
```lua
  local x : CFrame = CFrame.new()
  local y : Vector3 | CFrame
  if (math.random()) then
    y = CFrame.new()
  else
    y = Vector3.new()
  end
  local z : Vector3 | CFrame = x * y
```
then the type system checks that the type of `CFrame` multiplication is a subtype of `(CFrame, Vector3 | CFrame) -> (Vector3 | CFrame)`.

Subtyping is a very useful feature, and it supports rich type constructs like type union (`T | U`) and intersection (`T & U`). For example, `number?` is implemented as a union type `(number | nil)`, inhabited by values that are either numbers or `nil`.

Unfortunately, the interaction of subtyping with intersection and union types can have odd results. A simple (but rather artificial) case in older Luau was:
```lua
  local x : (number?) & (string?) = nil
  local y : nil = nil
  y = x -- Type '(number?) & (string?)' could not be converted into 'nil'
  x = y
```
This error is caused by a failure of subtyping, the old subtyping algorithm reports that `(number?) & (string?)` is not a subtype of `nil`. This is a false positive, since `number & string` is uninhabited, so the only possible inhabitant of `(number?) & (string?)` is `nil`.

This is an artificial example, but there are real issues raised by creators caused by the problems, for example <https://devforum.roblox.com/t/luau-recap-july-2021/1382101/5>. Currently, these issues mostly affect creators making use of sophisticated type system features, but as we make type inference more accurate, union and intersection types will become more common, even in code with no type annotations.

This class of false positives no longer occurs in Luau, as we have moved from our old approach of *syntactic subtyping* to an alternative called *semantic subtyping*.

## Syntactic subtyping

AKA “what we did before.”

Syntactic subtyping is a syntax-directed recursive algorithm. The interesting cases to deal with intersection and union types are:

* Reflexivity: `T` is a subtype of `T`
* Intersection L: `(T₁ & … & Tⱼ)` is a subtype of `U` whenever some of the `Tᵢ` are subtypes of `U`
* Union L: `(T₁ | … | Tⱼ)` is a subtype of `U` whenever all of the `Tᵢ` are subtypes of `U`
* Intersection R: `T` is a subtype of `(U₁ & … & Uⱼ)` whenever `T` is a subtype of all of the `Uᵢ`
* Union R: `T` is a subtype of `(U₁ | … | Uⱼ)` whenever `T` is a subtype of some of the `Uᵢ`.

For example:

* By Reflexivity: `nil` is a subtype of `nil`
* so by Union R: `nil` is a subtype of `number?`
* and: `nil` is a subtype of `string?`
* so by Intersection R: `nil` is a subtype of `(number?) & (string?)`.

Yay! Unfortunately, using these rules:

* `number` isn’t a subtype of `nil`
* so by Union L: `(number?)` isn’t a subtype of `nil`
* and: `string` isn’t a subtype of `nil`
* so by Union L: `(string?)` isn’t a subtype of `nil`
* so by Intersection L: `(number?) & (string?)` isn’t a subtype of `nil`.

This is typical of syntactic subtyping: when it returns a “yes” result, it is correct, but when it returns a “no” result, it might be wrong. The algorithm is a *conservative approximation*, and since a “no” result can lead to type errors, this is a source of false positives.

## Semantic subtyping

AKA “what we do now.”

Rather than thinking of subtyping as being syntax-directed, we first consider its semantics, and later return to how the semantics is implemented. For this, we adopt semantic subtyping:

 * The semantics of a type is a set of values.
 * Intersection types are thought of as intersections of sets.
 * Union types are thought of as unions of sets.
 * Subtyping is thought of as set inclusion.

For example:

| Type | Semantics |
|------|-----------|
| `number` | { 1, 2, 3, … } |
| `string` | { “foo”, “bar”, … } |
| `nil` | { nil } |
| `number?` | { nil, 1, 2, 3, … } |
| `string?` | { nil, “foo”, “bar”, … } |
| `(number?) & (string?)` | { nil, 1, 2, 3, … } ∩ { nil, “foo”, “bar”, … } = { nil } |


and since subtypes are interpreted as set inclusions:

| Subtype | Supertype | Because |
|---------|-----------|---------|
| `nil` | `number?` | { nil } ⊆ { nil, 1, 2, 3, … } |
| `nil` | `string?`| { nil } ⊆ { nil, “foo”, “bar”, … } |
| `nil` | `(number?) & (string?)` | { nil } ⊆ { nil } |
| `(number?) & (string?)` | `nil` | { nil } ⊆ { nil } |


So according to semantic subtyping, `(number?) & (string?)` is equivalent to `nil`, but syntactic subtyping only supports one direction.

This is all fine and good, but if we want to use semantic subtyping in tools, we need an algorithm, and it turns out checking semantic subtyping is non-trivial.

## Semantic subtyping is hard

NP-hard to be precise.

We can reduce graph coloring to semantic subtyping by coding up a graph as a Luau type such that checking subtyping on types has the same result as checking for the impossibility of coloring the graph

For example, coloring a three-node, two color graph can be done using types:

```lua
type Red = "red"
type Blue = "blue"
type Color = Red | Blue
type Coloring = (Color) -> (Color) -> (Color) -> boolean
type Uncolorable = (Color) -> (Color) -> (Color) -> false
```

Then a graph can be encoded as an overload function type with
subtype `Uncolorable` and supertype `Coloring`, as an overloaded
function which returns `false` when a constraint is violated. Each
overload encodes one constraint. For example a line has constraints
saying that adjacent nodes cannot have the same color:

```lua
type Line = Coloring
  & ((Red) -> (Red) -> (Color) -> false)
  & ((Blue) -> (Blue) -> (Color) -> false)
  & ((Color) -> (Red) -> (Red) -> false)
  & ((Color) -> (Blue) -> (Blue) -> false)
```

A triangle is similar, but the end points also cannot have the same color:

```lua
type Triangle = Line
  & ((Red) -> (Color) -> (Red) -> false)
  & ((Blue) -> (Color) -> (Blue) -> false)
```

Now, `Triangle` is a subtype of `Uncolorable`, but `Line` is not, since the line can be 2-colored.
This can be generalized to any finite graph with any finite number of colors, and so subtype checking is NP-hard.

We deal with this in two ways:

* we cache types to reduce memory footprint, and
* give up with a “Code Too Complex” error if the cache of types gets too large.

Hopefully this doesn’t come up in practice much. There is good evidence that issues like this don’t arise in practice from experience with type systems like that of Standard ML, which is [EXPTIME-complete](https://dl.acm.org/doi/abs/10.1145/96709.96748), but in practice you have to go out of your way to code up Turing Machine tapes as types.

## Type normalization

The algorithm used to decide semantic subtyping is *type normalization*.
Rather than being directed by syntax, we first rewrite types to be normalized, then check subtyping on normalized types.

A normalized type is a union of:

* a normalized nil type (either `never` or `nil`)
* a normalized number type (either `never` or `number`)
* a normalized boolean type (either `never` or `true` or `false` or `boolean`)
* a normalized function type (either `never` or an intersection of function types)
etc

Once types are normalized, it is straightforward to check semantic subtyping.

Every type can be normalized (sigh, with some technical restrictions around generic type packs). The important steps are:

* removing intersections of mismatched primitives, e.g. `number & bool` is replaced by `never`, and
* removing unions of functions, e.g. `((number?) -> number) | ((string?) -> string)` is replaced by `(nil) -> (number | string)`.

For example, normalizing `(number?) & (string?)` removes `number & string`, so all that is left is `nil`.

Our first attempt at implementing type normalization applied it liberally, but this resulted in dreadful performance (complex code went from typechecking in less than a minute to running overnight). The reason for this is annoyingly simple: there is an optimization in Luau’s subtyping algorithm to handle reflexivity (`T` is a subtype of `T`) that performs a cheap pointer equality check. Type normalization can convert pointer-identical types into semantically-equivalent (but not pointer-identical) types, which significantly degrades performance.

Because of these performance issues, we still use syntactic subtyping as our first check for subtyping, and only perform type normalization if the syntactic algorithm fails. This is sound, because syntactic subtyping is a conservative approximation to semantic subtyping.

## Pragmatic semantic subtyping

Off-the-shelf semantic subtyping is slightly different from what is implemented in Luau, because it requires models to be *set-theoretic*, which requires that inhabitants of function types “act like functions.” There are two reasons why we drop this requirement.

**Firstly**, we normalize function types to an intersection of functions, for example a horrible mess of unions and intersections of functions:
```
((number?) -> number?) | (((number) -> number) & ((string?) -> string?))
```
normalizes to an overloaded function:
```
((number) -> number?) & ((nil) -> (number | string)?)
```
Set-theoretic semantic subtyping does not support this normalization, and instead normalizes functions to *disjunctive normal form* (unions of intersections of functions). We do not do this for ergonomic reasons: overloaded functions are idiomatic in Luau, but DNF is not, and we do not want to present users with such non-idiomatic types.

Our normalization relies on rewriting away unions of function types:
```
((A) -> B) | ((C) -> D)   →   (A & C) -> (B | D) 
```
This normalization is sound in our model, but not in set-theoretic models.

**Secondly**, in Luau, the type of a function application `f(x)` is `B` if `f` has type `(A) -> B` and `x` has type `A`. Unexpectedly, this is not always true in set-theoretic models, due to uninhabited types. In set-theoretic models, if `x` has type `never` then `f(x)` has type `never`. We do not want to burden users with the idea that function application has a special corner case, especially since that corner case can only arise in dead code.

In set-theoretic models, `(never) -> A` is a subtype of `(never) -> B`, no matter what `A` and `B` are. This is not true in Luau.

For these two reasons (which are largely about ergonomics rather than anything technical) we drop the set-theoretic requirement, and use *pragmatic* semantic subtyping.

## Negation types

The other difference between Luau’s type system and off-the-shelf semantic subtyping is that Luau does not support all negated types.

The common case for wanting negated types is in typechecking conditionals:
```lua
-- initially x has type T
if (type(x) == "string") then
  --  in this branch x has type T & string
else
  -- in this branch x has type T & ~string
end
```
This uses a negated type `~string` inhabited by values that are not strings.

In Luau, we only allow this kind of typing refinement on *test types* like `string`, `function`, `Part` and so on, and *not* on structural types like `(A) -> B`, which avoids the common case of general negated types.

## Prototyping and verification

During the design of Luau’s semantic subtyping algorithm, there were changes made (for example initially we thought we were going to be able to use set-theoretic subtyping). During this time of rapid change, it was important to be able to iterate quickly, so we initially implemented a [prototype](https://github.com/luau-lang/agda-typeck) rather than jumping straight to a production implementation.

Validating the prototype was important, since subtyping algorithms can have unexpected corner cases. For this reason, we adopted Agda as the prototyping language. As well as supporting unit testing, Agda supports mechanized verification, so we are confident in the design.

The prototype does not implement all of Luau, just the functional subset, but this was enough to discover subtle feature interactions that would probably have surfaced as difficult-to-fix bugs in production.

Prototyping is not perfect, for example the main issues that we hit in production were about performance and the C++ standard library, which are never going to be caught by a prototype. But the production implementation was otherwise fairly straightforward (or at least as straightforward as a 3kLOC change can be).

## Next steps

Semantic subtyping has removed one source of false positives, but we still have others to track down:

* overloaded function applications and operators,
* property access on expressions of complex type, 
* read-only properties of tables,
* variables that change type over time (aka typestates),
* …

The quest to remove spurious red squiggles continues!

## Acknowledgments

Thanks to Giuseppe Castagna and Ben Greenman for helpful comments on drafts of this post.

## Further reading

If you want to find out more about Luau and semantic subtyping, you might want to check out…

* Luau. <https://luau-lang.org/>
* Lily Brown, Andy Friesen and Alan Jeffrey, *Goals of the Luau Type System*, Human Aspects of Types and Reasoning Assistants (HATRA), 2021. <https://arxiv.org/abs/2109.11397>
* Luau Typechecker Prototype. <https://github.com/luau-lang/agda-typeck>
* Agda. <https://agda.readthedocs.io/>
* Andrew M. Kent. *Down and Dirty with Semantic Set-theoretic Types*, 2021.  <https://pnwamk.github.io/sst-tutorial/>
* Giuseppe Castagna, *Covariance and Contravariance*, Logical Methods in Computer Science 16(1), 2022. <https://arxiv.org/abs/1809.01427>
* Giuseppe Castagna and Alain Frisch, *A gentle introduction to semantic subtyping*, Proc. Principles and practice of declarative programming (PPDP), pp 198–208, 2005. <https://doi.org/10.1145/1069774.1069793>
* Giuseppe Castagna, Mickaël Laurent, Kim Nguyễn, Matthew Lutze, *On Type-Cases, Union Elimination, and Occurrence Typing*, Principles of Programming Languages (POPL), 2022. <https://doi.org/10.1145/3498674>
* Giuseppe Castagna, *Programming with union, intersection, and negation types*, 2022. <https://arxiv.org/abs/2111.03354>
* Sam Tobin-Hochstadt and Matthias Felleisen, *Logical types for untyped languages*. International Conference on Functional Programming (ICFP), 2010. <https://doi.org/10.1145/1863543.1863561>
* José Valim, *My Future with Elixir: set-theoretic types*, 2022. <https://elixir-lang.org/blog/2022/10/05/my-future-with-elixir-set-theoretic-types/>

Some other languages which support semantic subtyping…

* ℂDuce <https://www.cduce.org/>
* Ballerina <https://ballerina.io>
* Elixir <https://elixir-lang.org/>
* eqWAlizer <https://github.com/WhatsApp/eqwalizer>

And if you want to see the production code, it's in the C++ definitions of [tryUnifyNormalizedTypes](https://github.com/Roblox/luau/blob/d6aa35583e4be14304d2a17c7d11c8819756beb6/Analysis/src/Unifier.cpp#L868) and [NormalizedType](https://github.com/Roblox/luau/blob/d6aa35583e4be14304d2a17c7d11c8819756beb6/Analysis/include/Luau/Normalize.h#L134) in the [open source Luau repo](https://github.com/Roblox/luau).
