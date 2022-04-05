# Function overload resolution

## Summary

The current algorithm for function overload resolution is designed for
greedy type inference. This RFC updates it for local type inference.

## Motivation

The algorithm for function overload resolution determines which type
to give `f(x)` for overloaded functions `f`. Currently it is as
follows (simplified, for example specializing to one argument and
result):

* Let `F` by the type of `f`, and `X` be the type of `x`.
* First flatten `F` to the form `F = F1 &...& FN`.
* For each `Fi`
 * If `Fi` is a function type `T -> U`, try unifying `X` with `T`. If unification succeeds, return `U`.
 * If `Fi` is a free type, unify it with `X -> Y` (for fresh free type `Y`) and return `Y`.
 * Otherwise, report an error.
* If we got here, overload resolution failed, so we bail and return the return type of one of the overloads.

For example:

* if `F` is `((number? -> string?) & (string? -> number?)` and `X` is free, then `X` is unified with `number?` and the result type is `string?`
* if `F` is `((number? -> string?) & (string? -> number?)` and `X` is `(number | string)`, then an error is reported and the result type is `string?`
* if `F` is `((number? -> string?) & (string? -> number?)` and `X` is `nil`, then the result type is `string?`

The order of constraints for this algorithm matters, for example:

```lua
  type T = { f : (number -> string) & (string -> number) }
  local function useT(x : T) x.f("hi") end
  local function (x)
    x.f(0)  -- this unifies the type of x.f with number -> Y
    useT(x) -- this does not typecheck because T gives a more precise type for f
  end
```

but swapping the order of constraints:
```lua
  type T = { f : (number -> string) & (string -> number) }
  local function useT(x : T) x.f("hi") end
  local function (x)
    useT(x) -- unifies the type of x with T
    x.f(0)  -- this is fine
  end
```

This is okay for greedy type inference, where the order of constraints
is important, but not great for local type inference, which is meant
to be independent of constraint order.

## Design

In this proposal, we delay producing a type for function applications until the function and argument types are concrete.

We do this by introducing types `src F` and `tgt F(X)` (syntax to be bikeshedded, but
hopefully it will never be seen by creators!), standing for the parameter and
result types of applying a function of type `F` to an argument of type `X`.

The algorithm for function overload resolution is now trivial: the type of `f(x)` is just `tgt F(X & src F)`, and this introduces a constraint `X <: src(F)`.

The extra work is now in type normalization, which removes uses of `src` and `tgt` during quantification.

First, we normalize `F & function`, resulting in a type `(S1 -> T1) & ... & (SN -> TN)`. During normalization, we *saturate* the type:

 * for any `(Si -> Ti)` and `(Sj -> Tj)` in the saturation, `((Si|Sj) -> (Ti|Tj))` is in the saturation, and
 * for any `(Si -> Ti)` and `(Sj -> Tj)` in the saturation, `((Si&Sj) -> (Ti&Tj))` is in the saturation.

Then:

* `src F` is (S1 | ... | SN)`
* `tgt F(X)` is the most precise `Ti` such that `X <: Si`.

If saturated types are ordered by subtyping order, this is the same as the current algorithm for function overload resolution.

For example saturating `(number? -> string?) & (string? -> number?)` gives `(nil -> nil) & (number? -> string?) & (string? -> number?) & ((number | string)? -> (number | string)?)` so:

```
  src((number? -> string?) & (string? -> number?))                    is  (number | string)?
  tgt((number? -> string?) & (string? -> number?)) (number)           is  string?
  tgt((number? -> string?) & (string? -> number?)) (number | string)  is  (number | string)?
  tgt((number? -> string?) & (string? -> number?)) (nil)              is  nil
```

(This is a variant of the algorithm given in https://www.irif.fr/~gc/papers/covcon-again.pdf)

The real thing would need scaled up to type packs and generic functions.

As an optimization, saturation can merge functions with the same
return types (since `(S1 -> T) & (S2 -> T)` is equivalent to `(S1 |
S2) -> T`). For example, the
unsaturated type for `CFrame.new` is

```
    (() -> CFrame)
  & ((Vector3, Vector3) -> CFrame)
  & ((number, number, number) -> CFrame)
  & ((number, number, number, number, number, number) -> CFrame)
  & ((number, number, number, number, number, number, number, number, number, number, number, number) -> CFrame)
```

saturating this would result in 32 overloads, but this can be optimized to one:

```
    ( ()
    | (Vector3, Vector3)
    | (number, number, number)
    | (number, number, number, number, number, number)
    | (number, number, number, number, number, number, number, number, number, number, number, number)
    ) -> CFrame
```

Hopefully this is a common case, since Roblox API function overloads all share a return type,
and we don't infer overloaded functions.

This does depend on having unions of type packs.

## Drawbacks

Anything we do about this will be a breaking change.

Scaling this up to type packs involves intersection and union on packs.

We need to ensure `src` and `tgt` do not leak.

We need to make sure that normalization doesn't lose valuable autocomplete information.

In this proposal, the type of `f(x)` can depend on the type of `x`, which may cause complexity in type inference.

There may be other devils in the details.

## Alternatives

We could try applying fixes to the current algorithm, and live with constraint order mattering.

We could adopt the TypeScript or Flow solution, which adds "if this then that" constraints, but that makes it very likely we will need to introduce additional backtracking.

We could normalize all types to have the same return type, by equating `(S1 -> T1) & (S2 -> T2)` with `((S1 | S2) -> (T1 & T2))`. This is a breaking change, though in a corner case I doubt has had much use. It does mean that `&` and `|` aren't just intersection and union any more, for example `(string -> number) & (number -> string)` contains

```lua
  function(x)
    if type(x) == "string" then
      return 1
    else
      return "hi"
    end
  end
```

which is not in `(string | number) -> never`.

The Roblox API does have some overloaded functions where the return
type depends on the argument type, for wexample `CFrame.__mul` has
type `(CFrame -> CFrame) & (Vector3 -> Vector3)`.
