# Type Error Suppression

## Summary

An alternative approach to type error suppression and the `any` type.

## Motivation

There are two reasons for this RFC: to make clearer how we're
approaching error suppression, and to remove the magic "both top and
bottom" behavior of the `any` type.

### Error suppression

Currently, we have ad hoc error suppression, where we try to avoid cascading errors, for example in
```lua
  local x = t.p.q.r
```

if `t` is a table without a `p` field, we report a type error on
`t.p`, but we avoid cascading errors by assigning `t.p` an internal
`error` type, and suppressing errors in property access `M.p` when `M`
has type `error`.

In this RFC, we clarify that error suppression occurs when the error
is caused by a type `T`, and `error` is a subtype of `T`.

### The `any` type

The `any` type is an outlier in the type system, in that currently it
is both a top type (`T` is a subtype of `any` for all types `T`) and a
bottom type (`any` is a subtype of `U` for all types `U`). This is
"consistent subtyping" (written `T ≾ U`) from Siek and Taha (2007),
which has the issue of not being transitive (if it were, then `T ≾ U`
for all types `T` and `U`, which is not a very useful definition of
subtyping).

The solution used by Siek and Taha is to split consistent subtyping (`S ≾ U`)
into a *consistency relation* `S ~ T` and a *subtyping relation* (`T <: U`).
The role of the consistency relation is to allow `any` to stand in for any type
(`any ~ T` for all types `T`).

We propose something different: performing *error suppression* on
failures of subtyping. We treat `any` as a top type, so `T <: any`,
but suppress type error messages caused by `any <: U` failing.

## Design

This design uses an `error` type (though adding user syntax for it is
out of scope of this RFC).

Call a type:

 * shallowly safe when any uses of `error` or `any` are inside a table or function type, and
 * deeply safe when it does not contain `error` or `any` anywhere.
 
A type `T` is shallowly unsafe precisely when `error <: T`.

We add a new subtyping relationship:

 * `any <: unknown | error`

We keep the existing subtyping relationships:

 * `T <: any` for any type `T`

We add a proviso to `unknown` being a top type:

 * `T <: unknown` for any *shallowly safe* type `T`

Currently, we consider a subtype test to have failed when it generates
no errors. We separate out the result of the check from its errors,
and instead have a requirement:

 * If checking `T <: U` succeeds, it produces no errors.

It is now possible for a subtyping test to fail, but produce no errors.
For example, `number <: any` succeeds (since `any` is the top type)
and `number <: string` fails with an error, but now `any <: string` fails
*but produces no errors*.

For end users, who only care about errors being reported, this will not be
a noticable change (but see the discussion of breaking changes below).
Internally though, it helps us avoid footguns, since now subtyping
is transitive.

The subtype testing algorithm changes:

 * Subtype checking returns a boolean.
 * Replace all of the current tests of "errors are empty" by testing the return value.
 * In the case of testing `any <: T`, return `true` with no errors.
 * In the case of testing `T <: any`, return `false` with no errors.
 * In the case of testing `T <: unknown`, check `T` for being a shallowly safe type.
 
These changes are not huge, and can be implemented for both the current greedy unifier,
and future constraint solvers.

Theses changes have been prototyped: https://github.com/luau-lang/agda-typeck/pull/4

## Drawbacks

This is theoretically a breaking change but my word you have to work hard at it.
For just checking subtyping there is no difference: the new algorithm returns `true` precisely
when the old algorithm generates no errors. But it can result in different unifications.

For example, if `Y` is a free type variable, then currently checking `(any & Y) <: number`
will not perform any unification, which makes a difference to the program:

```lua
  function f(x : any, y) -- introduces a new free type Y for y
    if x == y then       -- type refinement makes y have type (any & Y)
      return math.abs(y) -- checks (any & Y) <: number
    end
  end
```

Currently we infer type `<a>(any, a) -> number` for `f`. With the new
algorithm, checking `(any & Y) <: number` will succeed by unifying `Y`
with `number`, so `f` will be given the more accurate type
`(any, number) -> number`.

So this is a breaking change, but results in a more accurate type.
In practice it is unlikely that this change will do anything but help find bugs.

## Alternatives

We could implement Siek and Taha's algorithm, but that only helps with
`any`, not with more general error supression.

We could leave everything alone, and live with the weirdness of non-transitive subtyping.

