# Expanded Subtyping for Generic Function Types

## Summary

Extend the subtyping relation for function types to relate generic function
types with compatible instantiated function types.

## Motivation

As Luau does not have an explicit syntax for instantiation, there are a number
of places where the typechecker will automatically perform instantiation with
the goal of permitting more programs. These instances of instantiation are
ad-hoc and strategic, but useful in practice for permitting programs such as:

```lua
function id<T>(x: T): T
	return x
end

local idNum : (number) -> number
idNum = id -- ok
```

However, they have also been a source of some typechecking bugs because of how
they actually make a determination as to whether the instantation should happen,
and they currently open up some potential soundness holes when instantiating
functions in table types since properties of tables are mutable and thus need to
be invariant (which the automatic-instantiation potentially masks).

## Design

The goal then is to rework subtyping to support the relationship we want in the
first place: allowing polymorphic functions to be used where instantiated
functions are expected. In particular, this means adding instantiation itself to
the subtyping relation. Formally, that'd look something like:

```
instantiate(<a>(T1) -> T2) = (T1') -> T2'
(T1') -> T2' <: (T3) -> T4
--------------------------------------------
<a>(T1) -> T2 <: (T3) -> T4)
```

Or informally, we'd say that a generic function type is a subtype of another
function type if we can instantiate it and show that instantiated function type
to be a subtype of the original function type. Implementation-wise, this loose
formal rule suggests a strategy of when we'll want to apply instantiation.
Namely, whenever the subtype and supertype are both functions with the potential
subtype having some generic parameters and the supertype having none. So, if we
look once again at our simple example from motivation, we can walk through how 
we expect it to type check:

```lua
function id<T>(x: T): T
	return x
end

local idNum : (number) -> number
idNum = id -- ok
```

First, `id` is given the type `<T>(T) -> T` and `idNum` is given the type
`(number) -> number`. When we actually perform the assignment, we must show that
the type of the right-hand side is compatible with the type of the left-hand
side according to subtyping. That is, we'll ask if `<T>(T) -> T` is a subtype of
`(number) -> number` which matches the rule to apply instantiation since the
would-be subtype has a generic parameter while the would-be supertype has no
generic parameters. This contrasts with the current implementation which, before
asking the subtyping question, checks if the type of the right-hand side
contains any generics at any point and if the type of the left-hand side cannot
_possibly_ contain generics and instantiates the right-hand side if so.

Adding instantiation to subtyping does pose some additional questions still
about when exactly to instantiate. Namely, we need to consider cases like
function application. We can see why by looking at some examples:

```lua
function rank2(f: <a>(a) -> a): (number) -> number
    return f
end
```

In this case, we expect to allow the instantiation of `f` from `<a>(a) -> a` to
`(number) -> number`. After all, we can consider other cases like where the body
instead applies `f` to some particular value, e.g. `f(42)`, and we'd want the
instantiation to be allowed there. However, this means we'd potentially run into
issues if we allowed call sites to `rank2` to pass in non-polymorphic functions.
A naive approach to implementing this proposal would do exactly that because we
currently treat contravariant subtyping positions (i.e. for the arguments of
functions) as being the same as our normal (i.e. covariant) subtyping relation
but with the arguments reversed. So, to type check an application like
`rank2(function(str: string) return str + "s" end)` (where the function argument
is of type `(string) -> string`), we would ask if `<a>(a) -> a` is a subtype of
`(string) -> string`. This is precisely the question we asked in the original
example, but in the contravariant context, this is actually unsound since
`rank2` would then function as a general coercion from, e.g.,
`(string) -> string` to `(number) -> number`.

This sort of behavior does come up in other languages that mix polymorphism and
subtyping. If we consider the same example in F#, we can compare its behavior:

```fsharp
let ranktwo (f : 'a -> 'a) : int -> int = f
let pluralize (s : string) : string = s + "s"
let x = ranktwo pluralize
```

For this example, F# produces one warning and one error. The warning is applied
to the function definition of `ranktwo` itself (coded `FS0064`), and says "This
construct causes code to be less generic than indicated by the type annotations.
The type variable 'a has been constrained to be type 'int'." This warning
highlights the actual difference between our example in Luau and the F#
translation. In F#, `'a` is really a free type variable, rather than a generic
type parameter of the function `ranktwo`, as such, this code actually
constrains the type of `ranktwo` to be `(int -> int) -> (int -> int)`. As such,
the application on line 3 errors because our `(string -> string)` function is
simply not compatible with that type. With higher-rank polymorphic function
parameters, it doesn't make sense to warn on their instantiation (as illustrated
by the example of actually applying `f` to some particular data in the
definition of `rank2`), but it's still just as problematic if we were to accept
instantiated functions at polymorphic types. Thus, it's important that we
actually ensure that we only instantiate in covariant contexts. So, we must
ensure that subtyping only instantiates in covariant contexts.

It may also be helpful to consider an example of rank-1 polymorphism to
understand the full scope of the behavior. So, we can look at what happens if we
simply move the type parameter out in our working example:

```lua
function rank1<a>(f: (a) -> a): (number) -> number
    return f
end
```

In this case, we expect an error to occur because the type of `f` depends on
what we instantiate `rank1` with. If we allowed this, it would naturally be
unsound because we could again provide a `(string) -> string` argument (by
instantiating `a` with `string`). This reinforces the idea that the presence of
the generic type parameter is likely to be a good option for determining
instantiation (at least when compared to the presence of free type variables).

## Drawbacks

One of the aims of this proposal is to provide a clear and predictable mental
model of when instantiation will take place in Luau. The author feels this
proposal is step forward compared to the existing ad-hoc usage of instantiation
in the typechecker, but it's possible that programmers are already comfortable
with the mental model they have built for the existing implementation.
Hopefully, this is mitigated by the fact that the new setup should allow all of
the _sound_ uses of instantiation permitted by the existing system. Notably,
however, programmers may be surprised by the added restriction when it comes to
properties in tables. In particular, we can consider a small variation of our
original example with identity functions:

```lua
function id<T>(x: T): T
	return x
end

local poly : { id : <a>(a) -> a } = { id = id }

local mono : { id : (number) -> number }
mono = poly -- error!
mono.id = id -- also an error!
```

In this case, the fact that we're dealing with a _property_ of a table type
means that we're in a context that needs to be invariant (i.e. not allow
subtyping) to avoid unsoundness caused by interactions between mutable
references and polymorphism (see things like the [value
restriction in OCaml][value-restriction] to understand why). In most cases, we
believe programmers will be using functions in tables as an implementation of
methods for objects, so we don't anticipate that they'll actually _want_ to do
the unsound thing here. The accepted RFC for [read-only
properties][read-only-props] gives us a technically-precise solution since
read-only properties would be free to be typechecked as a covariant context
(since they disallow mutation), and thus if the property `id` was marked
read-only, we'd be able to do both of the assignments in the above example.

## Alternatives

The main alternatives would likely be keeping the existing solution (and
likely having to tactically fix future bugs where instantiation either happens
too much or not enough), or removing automatic instantiation altogether in favor
of manual instantiation syntax. The former solution (changing nothing) is cheap
now (both in terms of runtime performance and also development cost), but the
existing implementation involves extra walks of both types to make a decision
about whether or not to perform instantiation. To minimize the performance
impact, the functions that perform these questions (`isGeneric` and
`maybeGeneric`) actually do not perform a full walk, and instead try to
strategically look at only enough to make the decision. We already found and
fixed one bug that was caused by these functions being too imprecise against
their spec, but fleshing them out entirely could potentially be a noticeable
performance regression since the decision to potentially instantiate is one that
comes up often.

Removing automatic instantiation altogether, by contrast, will definitely be
"correct" in that we'll never instantiate in the wrong spot and programmers will
always have the ability to instantiate, but it would be a marked regression on
developer experience since it would increase the annotation burden considerably
and generally runs counter to the overall design strategy of Luau (which focuses
heavily on type inference). It would also require us to actually pick a syntax
for manual instantiation (which we are still open to do in the future if we
maintain an automatic instantiation solution) which is frought with parser
ambiguity issues or requires the introduction of a sigil like Rust's turbofish
for instantiation. Discussion of that syntax is present in the [generic
functions][generic-functions] RFC.

[value-restriction]: https://stackoverflow.com/questions/22507448/the-value-restriction#22507665 
[read-only-props]: https://github.com/Roblox/luau/blob/master/rfcs/property-readonly.md
[generic-functions]: https://github.com/Roblox/luau/blob/master/rfcs/generic-functions.md
