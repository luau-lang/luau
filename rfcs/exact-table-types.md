# Exact Table Types

## Summary

In Luau, we currently support two kinds of table types: sealed, inexact table types (the kind you
use as parameter types) and unsealed, exact table types (the kind the type inference engine uses
when checking table literals and subsequent assignments to them in the same scope). The RFCs for
[unsealed table types][unsealed-tables] use language like _precision_, but is unclear about their
exact meaning. This RFC aims to clarify the language and syntax around different facets of table
types, and proposes the introduction of sealed, exact table types and a migration plan to make them
the default table type.

## Motivation

In the current design of table types, we have conflated two axes of properties, namely whether
tables are sealed vs. unsealed and whether tables are exact vs. inexact. In the interest of both
making the language clearer and in making typechecking behavior easier to implement and explain, we
want to disentangle these concepts. For the sake of precision, this RFC aims to always specify both
facets, but in practice, we believe programmers will only really be concerned with whether or not a
table type is exact.

The first distinction, sealed vs. unsealed, deals with state during typechecking. Namely, a _sealed_
table type is one that will not be changed during typechecking. So, if we have a binding with a
sealed table type, we know that the type of the binding itself will not change. By contrast,
_unsealed_ table types are stateful, and an assignment to a binding can grow the set of properties
on that type. All unsealed tables are sealed when they leave the scope they were created in, so
unsealed tables should be thought of as a technique used for typechecking imperative-style table
initialization code.

The second distinction, exact vs. inexact, deals with how the table type relates to other table
types. In particular, an exact table type indicates that the table has _only_ the properties listed
in its type, while an inexact table type indicates that the table has _at least_ the properties
listed in its type. This corresponds to whether or not _width subtyping_ is allowed on the type.

With this language, we currently have two table types which are determined contextually, and have no
syntactic difference. When a table type is used as the type of a function parameter, e.g. `function
getx(t : {x: number}) return x end`, we treat is as _sealed_ and _inexact_ meaning the type of the
parameter will _not_ change, but call sites to the function will permit tables that have _more_
properties than the ones listed. Within the body of the function however, the typechecker should
only permit the use of properties explicitly listed in the type since it was annotated. If the type
was instead inferred, a dual condition should be true true --- the properties explicitly listed in
the type are precisely the ones whose existence the function depends on. Meanwhile, when a table
type is used for a local definition, it is given an _unsealed_ and _exact_ type meaning we always
know _precisely_ the set of properties it has, but that the type itself is stateful and assignments
are allowed to grow the set of properties the table has.

This alludes to a gap of two sorts of table types: __sealed, exact tables_ and _unsealed, inexact
tables_. We believe the latter is undesirable in the sense that there does not appear to be a
compelling use case for _forgetting_ some of the structure of the table you're building statefully.
The main consequence of doing so would be that you would be unable to make further assignments to a
property after forgetting it was present. The former, however, is the novel proposal for this RFC.

## Design

A _sealed, exact table_ type is effectively the most strict we could be about a particular table. It
says that the table does not allow width subtyping, and therefore must have _exactly_ the properties
described by the type, and further says that the type itself is final meaning assignments to missing
properties would subsequently be rejected. To imagine why this is useful, we can consider a small
fragment of a hypothetical vector math library:

```lua
type Vec2 = { x: number, y: number }
type Vec3 = { x: number, y: number, z: number }

function length(pt: Vec2)
  return math.sqrt(pt.x^2 + pt.y^2)
end
```

If the table type `Vec2` here is `inexact`, we would allow `length` to be applied to a table of type
`Vec3`. However, in this case, our domain knowledge tells us that `length`'s behavior is incorrect
for `Vec3` since it does not take into account the `z`-component. While we could design a function
that instead worked for both, it may be reasonable to instead say that this function only operates
on the _exact_ table. While this particular example is admittedly a bit contrived,
[similar proposals][ts-exact-types] for other languages like TypeScript have collected many examples
where API designers wanted to be able specify exact types. One general pattern of use cases is 
state machines where you might define a number of types representing individual states and then a
union over all such types. With only inexact types, the type system permits undesirable combinations
that can introduce unwanted behavior in code using the state machine.

```luau
type StartState = {
  state: "Start",
  startData: number,
}

type OtherState = {
  state: "Other",
  otherData: string,
}

type State = StartState | OtherState

-- but we can construct a `State` that is neither a `StartState` nor an `OtherState`.
local notRight: State = { state: "Start", startData: 42, otherData: "foo" }
```

Since this proposal would involve having two sorts of sealed table types, _exact_ and _inexact_, and
their difference is not something we can distinguish contextually in general (as was the case was
_sealed_ and _unsealed_), we have to propose a syntax for them. This proposal posits that the best
path forward is to make sealed, exact tables the default table that you get when you write, e.g. `{
x: number, y: number }` as an annotation on a parameter or in a type alias. To annotate a table as
inexact and therefore open to extension, we would include an ellipsis in the type, e.g. `{ x:
number, y: number, ...}`. This was originally proposed as an alternative design in the [width
subtyping][width-subtyping] proposal.

There are a few apparent benefits to this approach. First, it means that a programmer designing an
API has to make a conscious decision that "yes, in fact, it is sensible to treat all of the unlisted
properties of this parameter as being unrelated to the behavior of this function." The type checker
should prefer approaches that, by default, rule out more possible bugs in API usage while keeping
explicit acceptance of the looser behavior natural and low friction. The motivation for taking this
approach really being that, as a programmer, you are _forced_ to notice when a program you want to
succeed produces an error, but you can remain blissfully unaware when programs you want to be
rejected succeed silently.

Secondly, though relatedly, making all table types inexact-by-default seems to exacerbate confusions
when considering both the meaning of and the subtyping relations between table types. Consider, for
example, the inexact table type `{...}` (currently written `{}` in the inexact-by-default world).
Right away, the syntax of the type matches the syntax of the value form for the empty table. This
makes it tempting to think of the type as something like "the empty table type" which one might
expect has a single inhabiting value, namely the empty table. However, with inexact-by-default, this
`{}` type is really the type of _every single table_ and it functions as a sort of top type for
tables. If you do mistakenly read it (even momentarily) as the empty table type, you can then be
surprised by the fact that it is _not_ a subtype of any more specific table, such as `{ x: number,
y: number }`. This makes sense with the understanding in mind that it is the type of _all_ tables,
rather than just the empty table, but then you may be surprised that it _is_ a subtype of `{[any]:
any}`, but is not a subtype of, say, `{[any]: number}`. These sorts of surprises and confusions have
come up in internal discussions of subtyping for Luau, and the syntax appears to not be doing us any
favors here.

Thirdly, we argue that it gives the most coherent and intuitive syntax for distinguishing exact vs.
inexact tables since we believe that being able to distinguish between them and choose the
appropriate one is important. Ellipses have an already established connotation of omission which
maps exactly to how sealed, inexact types are _omitting_ some of the properties that the tables
actually have (since tables with arbitrarily many additional properties can be accepted at an
inexact table type). The author is unaware of existing syntax that carries the same strong
connotation, but for exactness. One proposal discussed in the alternatives would be to follow Flow
in using `{| x: number, y: number |}` to mean a sealed, exact table.

Though we are proposing to make the default interpretation of table _annotations_ sealed and exact,
we maintain that the sensible default inference behavior is to default to inferring inexact table
types for unannotated table parameters. Consider the following example:

```lua
function dostuff(t)
  ...
  t.x = 4
  ...
  t.y = 5
  ...
end
```

In this case, we expect that `t` will be given the inferred type `{x: number, y: number, ...}`.
However, if we also passed `t` to another function that requires an exact table, we can propagate
this requirement to `dostuff`.

```lua
function dostuff(t)
  ...
  t.x = 4
  ...
  t.y = 5
  ...
  dootherstuff(t)
  ...
end

function dootherstuff(pt: {x: number, y: number})
  ...
end
```

So, in this case, `t` would be given the exact table type `{x: number, y: number}` during inference.
Note that this is still true even if `dostuff` did not refer to some or all of the properties of `t`
outside of passing it into `dootherstuff`. That is, the type will be the same even if the code was
as follows:

```lua
function dostuff(t)
  return dootherstuff(t)
end

function dootherstuff(pt: {x: number, y: number})
  ...
end
```

If instead the table is passed to a function requiring an exact table type with _different_
properties than the ones used in `dostuff`, we will emit an error indicating that they are in
conflict.

```lua
function dostuff(t)
  ...
  t.z = 4
  ...
  ...
  dootherstuff(t)
  ...
end

function dootherstuff(pt: {x: number, y: number})
  ...
end
```

In this case, `t` has two constraints: one indicating that it should be the exact table type
`{x: number, y: number}` and the other indicating that it should have the property `z`. These
constraints cannot be reconciled, and so we will get an error at the call site of `dootherstuff`.

## Drawbacks

The main drawback of doing this is that the existing RFC for [sealed table
subtyping][width-subtyping] was already implemented as of March 1st, meaning that this is ultimately
a breaking change. As such, it would be important to manage the change as a gradual rollout to
mitigate problems. Since Luau currently only has sealed, inexact table types we can start by adding
the syntax for explicit inexact tables (e.g. `{x: number, y: number, ...}`) and implement a warning
that explains the default behavior will change in the future and to change it to the explicit
inexact syntax if the programmer would like to keep the current behavior. At a later date, we can
enable exact-by-default, but should provide an explicit suggestion in the error message to tell the
programmer that if they _intended_ to allow tables with additional properties, they can change the
signature of the function to include the `...`.

## Alternatives

There are two primary alternatives to this proposal. First, we could stick with the decision that
was already made in the [sealed table subtyping][width-subtyping] proposal and leave sealed, inexact
tables as the default table type. We then have either the choice of doing nothing else or adding
a new type syntax for sealed, exact tables. If we do nothing, we are reducing the ability for API
designers to rule out certain classes of bugs like the one presented with vector types. The
implicit assumption of width subtyping, after all, is that properties not listed in the type are
orthogonal to the function using that type, but of course, this assumption is sometimes wrong. It is
possible still that inexactness is so often necessary that we see little safety benefit from making
exactness available. If we add a new syntax for sealed, exact tables instead, the most sensible
syntax would likely follow Flow in including pipes, so `{| x: number, y: number |}` would be the
type of a sealed, exact table. This approach does not require a migration path, but would likely
reflect the belief that the added safety of exact table types is only rarely necessary. The author
believes it is worth considering, however, that Flow has also been migrating to
[exact-by-default][flow-exact-by-default] for their object types.

[unsealed-tables]: https://github.com/Roblox/luau/blob/master/rfcs/unsealed-table-literals.md
[ts-exact-types]: https://github.com/microsoft/TypeScript/issues/12936
[width-subtyping]: https://github.com/Roblox/luau/blob/master/rfcs/sealed-table-subtyping.md
[flow-exact-by-default]: https://medium.com/flow-type/how-to-upgrade-to-exact-by-default-object-type-syntax-7aa44b4d08ab
