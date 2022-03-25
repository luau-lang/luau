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

The algorithm for function overload resolution is now trivial: the type of `f(x)` is just `tgt F(X)`, and this introduces a constraint `X <: src(F)`.

The extra work is now in type normalization, which removes uses of `src` and `tgt` during quantification:

```
  src (T -> U) is  T 
  src (T & U)  is  src T | src U
  src (T | U)  is  src T & src U
  src T        is  src F when T is a table with a metatable with a __call__ property of type F
  src T        is  none in any other case

  tgt (T -> U) (V) is  U    when (T&V) is inhabited
  tgt (T & U) (V)  is  tgt T(V) & tgt U(V)
  tgt (T | U) (V)  is  tgt T(V) | tgt U(V)
  tgt T (V)        is  tgt F(V) when T is a table with a metatable with a __call__ property of type F
  tgt top (T)      is  top
  tgt T (V)        is  none in any other case
```

For example:

```
  src((number? -> string?) & (string? -> number?))                    is  (number | string)?
  tgt((number? -> string?) & (string? -> number?)) (number)           is  string?
  tgt((number? -> string?) & (string? -> number?)) (number | string)  is  (number | string)?
  tgt((number? -> string?) & (string? -> number?)) (nil)              is  nil
```

(This is a variant of the algorithm used in the Luau prototype.)

The real thing would need scaled up to type packs and generic functions.

## Drawbacks

Anything we do about this will be a breaking change.

Scaling this up to type packs probably involves intersection and union on packs.

There may be other devils in the details.

We need to ensure `src` and `tgt` do not leak.

We need to make sure that normalization doesn't lose valuable autocomplete information.

We need an algorithm for "is a type inhabited".

## Alternatives

We could try applying fixes to the current algorithm, and live with constraint order mattering.

We could adopt the Flow solution, which adds "if this then that" constraints, but that makes it very likely we will need to introduce additional backtracking.
