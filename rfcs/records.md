# Records

## Summary

Introduces a new data type, record, which is essentially a dictionary with fixed structure.

## Motivation

Lua has tables as the only data structure. Tables are very versatile - they can model arrays, dictionaries, tuples, objects, etc.
With metatables, they can also model rich OOP patterns like inheritance or properties.

This flexibility comes at a cost - using tables for everything means that for every specific usecase tables are likely suboptimal.

A lot of data in Luau programs uses tables as objects - tables with more or less rigid set of string keys. Sometimes these objects have
metatables that define methods, sometimes they overload operators, sometimes they are just used to store data.

Tables aren't perfect for objects; this proposal is motivated by the following deficiencies in using tables-as-objects (in rough priority order):

1. Memory overhead. An object with 6 fields takes `8+56+8+8*32=328` bytes of storage.
This is because tables have a large header, the hash portion is rounded to a power of two, and every entry has key and value despite the fixed object structure.
2. Type system compatibility. While Luau type checker can type tables, including ones with rigid structure, it falls short in OOP scenarios because it's very difficult
to associate methods with table structure in idiomatic OOP in Luau with tables. Today the situation is especially dire because each method gets its own inferred self type
(something that is likely to change), and it's impossible to specify a table-with-metatable type via obvious type syntax.
3. Runtime overhead. While we heavily optimize table reads and writes in object-like scenario, there's still a non-zero cost that is paid for establishing the mapping
from the index literal to the hash field. Method calls are similarly optimized but the optimization is restricted by the generality of table structure. Finally, large
object size results in higher cache pressure which affects performance.
4. Strictness of access. Today at runtime, reading an unknown key from the table returns `nil` and writing a new key just works. This creates significant amount of
complexity in the type checker, as it has to differentiate between tables that are open for extension and closed via a set of heuristics, and results in easy to make mistakes in untyped code.

Today, writing idiomatic objects in Luau is relatively straightforward but making idiomatic OOP type safe or maximally efficient is next to impossible.

## Design

This proposal suggests solving these problems with a new data type, called record. Record is a type that is semantically a dictionary with a fixed set of keys; much like
a table, values associated with keys can be read or written to. Much like a table, it has a metatable that can be used to customize behavior of the object by providing
extra operators (for arithmetics, stringification, etc.), as well as specifying methods.

The difference between records and tables is that when creating a record, the VM allocates space for all values of all keys without having to duplicate the key data;
since the structure of a record is immutable even if the contents isn't, this allows a much more efficient representation. A 6-field record will take estimated `8+32+6*16=136`
bytes without implementation heroics, which is more than twice as efficient as table storage. For applications that use many objects this has potential to halve
the memory footprint.

Record fields use flexible types at runtime (we always allocate space for TValue and don't restrict writes into the record to a given type). In the future, we may
introduce support for packed records where the table definition must use types and writes that don't abide by these types will trigger an error.

> TODO: It's going to be difficult to migrate to packed records. Is there anything we can do right now to keep this possibility open without enforcing types at runtime?

The rest of this proposal goes into syntax and semantics. The goal of this proposal is to solve the problem of object storage both for simple objects and for classes -- that is, if we add records we won't need to add classes.

### Record type

A record is a collectable object that stores the field values as a inline array of values (TValues) as well as a pointer to the shape. Shape is a table
that stores various lookup data as implementation details as well as metafields. `type(r)` is `"record"`; `getmetatable(r)` can be used to retrieve the shape.

Being a collectable object, records use raw equality by default when comparing using `==` or hashing; equality behavior can be overridden via `__eq`. From this
perspective, one could think of records as a user-defined userdata type as opposed to a host-defined userdata type: both typically expose a strict set of fields,
both are heap-allocated, both use contiguous storage.

Shape contains field lookup data in extra storage that's only allocated for shape tables, as well as the regular table fields. The field lookup data is internal and
immutable; for example, it might contain a string->index dictionary to be able to quickly locate fields in internal storage.

> TODO: Still not fully set on whether we can get by without a first class shape type.

Reading and writing fields from a record uses `.` or `[]` operator; like tables, passing the field name returns the field value. Unlike tables, if the field is not
present in the table, the error is raised. This is in constrast with tables where `nil` is returned for unknown keys upon read; records are meant to be stricted than
tables and as such returning `nil` will mask valuable errors, and make it more difficult to be strict about the types of the result.

Invoking methods with `:` desugars into `getmetatable(obj).__index.method(obj, args)` instead of the usual `obj.method(obj, args)`. This is important because it allows to
keep the method calls as efficient as possible, as they don't need to check whether the object has a given method.

> TODO: Should we use `__namecall` instead of `__index`? It seems more consistent, but at the same time `__namecall` today expects a function so it might be best
> to leave it as is?

> TODO: How do we expose the record keys? Should it be a builtin? Accessible through shape? Not available initially?

### Defining and constructing records

To define a record, you need to create the shape, which you can do using the newly introduced syntax with a context-sensitive `record` keyword:

Syntax A:

```
record Person = { name: string, age: number }

-- types can be omitted
record Point = { x, y }
```

Syntax B:

```
record Person(name: string, age: number)
-- types can be omitted
record Point(x, y)
```

This defines `Point` simultaneously as a local variable that corresponds to the shape table, and a type variable that corresponds to the record type.

The resulting shape table automatically is set up to be a valid record shape, but can still be modified by adding methods to it:

```
function Point.__add(l, r)
    return Point(l.x + r.x, l.y + r.y)
end

function Point:sum()
    return self.x + self.y
end

function Point.newDiagonal(v)
    return Point(v, v)
end
```

Note that `Point` is simply a table and as such it can be used to store static methods as well; as it also serves as a metatable, metafields defined on this table
will change the behavior of the record values. The shape isn't frozen automatically but can be frozen manually if desired via `table.freeze`.

To create a record, you need to use a record constructor. This is where the draft design has four options, each goes with one of the syntax options.

A1. Creation uses special syntax, `new Record { field = value ... }`. This is unsurprising and easy to implement, but verbose.
A2. Creation uses existing Lua DSL syntax `Record { field = value, ... }`. This is concise but requires a slightly intricate bytecode design to keep efficient.
B1. Creation uses special syntax, `new Record(value, ...)`. This is unsurprising and easy to implement, but verbose.
B2. Creation uses existing Lua call syntax, `Record(value, ...)`. This is concise and reasonably easy to keep efficient.

The big difference between variants A and B is whether you need to spell out field names at construction time. There's precedents for going either way; some
languages like F#/C#/Kotlin implement record construction as a function call and when you define a record, you essentially define the record constructor. This
is beautifully concise, but is a bit more difficult to migrate away from tables, and it's easy to mix up the names. Variants A are more verbose and either require
an extra `new` context-specific keyword, or complex/awkward magic to keep construction efficient.

In variants A, it would make sense to allow omission of any field, in which case it gets replaced with the default of `nil`. A future extension (not part of this RFC) could be made to
allow specification of default values at record definition time. Type checker would fail to type check record construction if fields that have non-optional types
have the values omitted.

In variants B, it would probably make sense to require exact number of values to be specified, or follow the usual function call syntax rules.

Note that since records are first class objects, you can export or import a record through a module boundary in the usual way:

```
local X = require(path).X
local r = new X(1, 2)
```

### Generic records

> TODO: This needs some thought; e.g., do we support explicit specification of record arguments at construction time and what's the syntax for that? Do we need to do this in the first proposal? This likely makes packed records effectively impossible to support at runtime without a huge amount of work, does that matter?

### Type checking records

Records defined via a `record` statement can be used in type annotations as usual. The unification rules say that a table can unify with a record if the fields
match, which makes records similar to sealed tables from the type checking perspective. (note, this is hand wavy on subtyping rules)

> TODO: How do you export a record type? `export record` would be straightforward but potentially conflicts with future export statements for functions/values.
> Alternatively, is `export type Record = Record` too awkward?

The methods defined on the record object are type checked as usual, with one exception - the implicit `self` has the type of the record. This is crucial because
this is the one big issue we haven't yet resolved with metatable-based OOP for tables, but it works for records because `:` is slightly more magical.

When `self` is explicit, the type needs to be specified manually, e.g. these definitions are equivalent:

```
function Point:sum(): number
    return self.x + self.y
end

function Point.sum(self: Point): number
    return self.x + self.y
end
```

Of course, the type checker also knows that the record type has the metatable with the inferred type of the record shape.

> TODO: Does the type checker need to understand the internal structure of the shape so that type checking works across modules, or is simply modeling this
> as a metatable sufficient?

### Object modeling

> TODO: records don't support implementation inheritance and why it's a good thing

## Drawbacks

Adding a new data type that is cross-cutting (across syntax, semantics/compiler, semantics/type checking, and runtime) results in added complexity.

The rigidity of records may make some applications hesitate to adopt them; e.g. you can't simply add a new field at a random point in the program, which some would
argue makes the language less dynamic and therefore less convenient.

Not enforcing type compatibility for typed records at runtime may make it difficult for us to optimize record storage more by removing the type tags (which could
make record objects ~2x more efficient in some cases).

## Alternatives

Instead of using explicit record types, we can make the VM recognize shapes of objects automatically, just like JavaScript implementations do. This requires a
substantial amount of complicated machinery and heuristics, and likely can't be as efficient as records in the long run, but it can result in close efficiency
without any changes to existing programs. This, however, doesn't make type safety any easier.

Instead of using record types that have minimal featureset, we could implement classes that have a more feature-rich OOP semantics, with inheritance, first class
properties, and access control. This would better map to other high level languages like TypeScript/Python, but would make the language and runtime more complicated.

Instead of allowing records to have metatables, we could have separate dedicated storage for methods and come up with a new scheme for operator overloading. This
would better map to other high level languages like C++ or C#, but would make the language less consistent.

Instead of defining records separately from arrays, we could define interactions between records-stored-inside-arrays (achieving single-allocation arrays of compound objects) and arrays-stored-inside-records (making it possible to store a fixed size array in a record). Both of these really aren't compatible with TValue storage and result in dramatically higher implementation effort.
