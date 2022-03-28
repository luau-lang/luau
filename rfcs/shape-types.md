# Shape types (concept name TBD)

## Summary

Introduce shape types, which are like table types, but are inhabited by both tables and class instances.

## Motivation

Currently we have no way to specify the type of a class instance with
an extra property. The obvious candidate is something like `Part & { p
: number }`, but currently this is uninhabited, since table types only
contain tables, not class instances. With this proposal, `Part & { p :
number }` will be inhabited by instances of a subclass of `Part` which
have a number property `p`, as you might expect.

Currently, generic and free tables have different typing rules.
Generic and free table types can be inhabited by class objects,
but sealed and unsealed tables cannot.

## Design

Rename table types to shape types.

Allow a class instance to be a member of a shape type if has all the named properties, and they have a matching type.
(This is no change for generic and free tables, but makes sealed and unsealed tables act the same as free tables).

Allow a class to be a subtype of a table type if has all the named properties, and they have a matching type.
(This is also no change for generic and free tables, but makes sealed and unsealed tables act the same as free tables).

With these changes, `Part & { p : number }` is now an inhabited type, which it currently is not.

Note that we can recover the current semantics if we also adopt the [Function and table types RFC](function-and-table-types.md),
since the current semantics of `{ p : number }` is `table & { p : number }`.

We should audit the built-in APIs to check which ones require tables and which ones are also prepared to accept class instances.

This design will future-proof us for bounded generics, for example the inferred type of

```lua
  function getP(x) return x.p end
```

will, with bounded generics and this RFC, have type:

```lua
  getP : <a, t <: {p:a}> (t) -> a
```

This design is essentially the same as
[WebIDL](https://webidl.spec.whatwg.org/#idl-objects), where what
we're calling shape types they call "callback interfaces".

TypeScript is a bit vague about platform objects, but the [DOM
bindings](https://github.com/microsoft/TypeScript/blob/main/lib/lib.dom.d.ts)
use interface types, so presumably TS interfaces are inhabited by both
platform objects and ECMAScript objects.

We should bikeshed the concept name: table types? Tably types? Shape types? Interface types?

## Drawbacks

This is a breaking change.

## Alternatives

Introduce shape types, but introduce different syntax for them.

Leave things alone.

