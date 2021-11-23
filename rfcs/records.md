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

Today, writing idiomatic objects in Luau is relatively straightforward but making idiomatic OOP type safe or maximally efficient is very difficult.

## Design

This proposal suggests solving these problems with a new data type, called record. Record is a type that is semantically a dictionary with a fixed set of keys; much like
a table, values associated with keys can be read or written to. Much like a table, it has a metatable that can be used to customize behavior of the object by providing
extra operators (for arithmetics, stringification, etc.), as well as specifying methods.

Syntactically, the difference between records and tables is that defining the record simultaneously creates a type name for the record and a shape object for the record,
that acts as a metatable and as a method table - this creates a single straightforward path for the users of the language to talk about objects with methods. In addition, we
constrain the expected types for the methods in such a way that the connection between the record shape, the record methods, and the self type in those methods is defined
a-priori, instead of us having to extract this information with heuristics that assume a specific metatable-based source assembly.

In addition, a record object always has the final shape - it's impossible to create a record with missing fields, at least from the type checker perspective. This makes
it easier to reason about the record types without having to model type states and without complex issues around method calls to partially complete tables.

At runtime, the difference between records and tables is that when creating a record, the VM allocates space for all values of all keys without having to duplicate the key data;
since the structure of a record is immutable even if the contents isn't, this allows a much more efficient representation. A 6-field record will take estimated `8+32+6*16=136`
bytes without implementation heroics, which is more than twice as efficient as table storage. For applications that use many objects this has potential to halve
the memory footprint.

Due to carefully specified access rules, records can be more efficient than tables as far as performance is concerned even without having type information at runtime.
In the future, type feedback from the type checker into the compiler will allow us to implement even more efficient record access, especially when combined with native
code generation -- and that implementation will not require complex shape caches and handling invalidation with associated deoptimizations and performance cliffs.

Record fields use flexible types at runtime (we always allocate space for TValue and don't restrict writes into the record to a given type). In the future, we may
introduce support for packed records where the table definition must use types and writes that don't abide by these types will trigger an error

The rest of this proposal goes into syntax and semantics. The goal of this proposal is to solve the problem of object storage both for simple objects and for classes -- that is, if we add records we won't need to add classes.

### Record type

A record is a collectable object that stores the field values as a inline array of values (TValues) as well as a pointer to the shape. Shape is a table
that stores various lookup data as implementation details as well as metafields.

`type(r)` is `"record"`; `getmetatable(r)` can be used to retrieve the shape. `typeof(r)` is `$` followed by the name of the record as spelled in the source file.
The prefix is required to make sure that builtin object types like `number` or host-provided userdata like `Vector3` can't be spoofed and confused with records.

> TODO: Why `$`? :)

Being a collectable object, records use raw equality by default when comparing using `==` or hashing; equality behavior can be overridden via `__eq`. From this
perspective, one could think of records as a user-defined userdata type as opposed to a host-defined userdata type: both typically expose a strict set of fields,
both are heap-allocated, both use contiguous storage.

Shape contains field lookup data in extra storage that's only allocated for shape tables, as well as methods and metamethods stored as regular table entries.
The field lookup data is internal and immutable; for example, it might contain a string->index dictionary to be able to quickly locate fields in internal storage.

Reading and writing fields from a record uses `.` or `[]` operator; like tables, passing the field name returns the field value. Unlike tables, if the field is not
present in the table, the error is raised. This is in constrast with tables where `nil` is returned for unknown keys upon read; records are meant to be stricted than
tables and as such returning `nil` will mask valuable errors, and make it more difficult to be strict about the types of the result.

The field lookup does not use `__index` or `__newindex` metamethods (or the metatable in general).

Invoking methods with `:` desugars into `getmetatable(obj).method(obj, args)` instead of the usual `obj.method(obj, args)`. This is important because it allows to
keep the method calls as efficient as possible, as they don't need to check whether the record has a given method as a field.

> TODO: Should we use `__namecall` instead of raw MT access? It seems more consistent, but at the same time `__namecall` today expects a function so it might be best
> to leave it as is?

> TODO: How do we expose the record keys? Should it be a builtin? Accessible through shape? Not available initially?

### Defining and constructing records

To define a record, you need to create the shape, which you can do using the newly introduced syntax with a context-sensitive `record` keyword.

> TODO: The draft RFC suggests two options for the syntax; only one will be chosen in the final version

Syntax A:

```lua
record Person = { name: string, age: number }

-- types can be omitted and default to any
record Point = { x, y }
```

Syntax B:

```lua
record Person(name: string, age: number)
-- types can be omitted and default to any
record Point(x, y)
```

This defines `Point` simultaneously as a local variable that corresponds to the shape table, and a type variable that corresponds to the record type.

The resulting shape table automatically is set up to be a valid record shape, but can still be modified by adding methods to it:

```lua
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

To create a record, you need to use a record constructor. This is done using call-like syntax:

Syntax A:

```lua
local person = Person { name = "Bob", age = 42 }
```

Syntax B:

```lua
local person = Person("Bob", 42)
```

The big difference between variants A and B is whether you need to spell out field names at construction time. There's precedents for going either way; some
languages like F#/C#/Kotlin implement record construction as a function call and when you define a record, you essentially define the record constructor. This
is beautifully concise, but is a bit more difficult to migrate away from tables, and it's easy to mix up the names. Variant A is more verbose and requires a bit
more magic at compile time to keep construction efficient.

In variant A, it would make sense to allow omission of any field, in which case it gets replaced with the default of `nil`. A future extension (not part of this RFC) could be made to
allow specification of default values at record definition time. Type checker would fail to type check record construction if fields that have non-optional types
have the values omitted.

In variant B, it would probably make sense to require exact number of values to be specified, or follow the usual function call syntax rules.

Note that since records are first class objects, you can export or import a record through a module boundary in the usual way:

```lua
local HR = require(path)
local r = HR.Person { name = "Bob", age = 42 } -- or HR.Person(1, 2) in variant B
```

### Generic records

At definition point, records can have generic arguments that can be used in the field type specification:

```lua
record Point<V> = { x: V, y: V }
```

When record names are used in type context, they use the standard generic instantiation syntax to specify the generic parameters:

```lua
local p: Point<number>
```

When record names are used in record literals, they don't specify the generic parameters. This is to avoid complexity with parsing `<` in expression context:

```lua
local p: Point<number> = Point { x = 1, y = 2 }
```

The generic type parameters are erased at runtime.

### Type checking records

Records defined via a `record` statement can be used in type annotations.

> TODO: How do you export a record type? `export record` would be straightforward but potentially conflicts with future export statements for functions/values.
> Alternatively, is `export type Record = Record` too awkward?

The methods defined on the record object are type checked as usual, with one exception - the implicit `self` has the type of the record. This is crucial because
this is the one big issue we haven't yet resolved with metatable-based OOP for tables, but it works for records because `:` is slightly more magical.

When `self` is explicit, the type needs to be specified manually, e.g. these definitions are equivalent:

```lua
function Point:sum(): number
    return self.x + self.y
end

function Point.sum(self: Point): number
    return self.x + self.y
end
```

Of course, the type checker also knows that the record type has the metatable with the inferred type of the record shape. This gives us an advantage in that
the use of the record type, whether explicit or inferred (via self), automatically gets access to both the correct definition of fields - which is specified
explicitly and as such is correct - as well as the full definition of methods.

### Subtyping rules

Given two record types, or a record and a table, how do we know whether one is a subtype of another? This brings up the question of whether records are nominal or structural.
Note that this doesn't affect the behavior of record types at runtime, but does affect typechecking semantics.

Today Luau type system supports structural types, including table types with fixed structure (sealed tables), as well as nominal types (classes) used to model host API (userdata).

Records could either be modeled as a structural construct like a sealed table, or as a nominal construct like a class.

In the former case, record is a subtype of another record if the fields are a superset of the fields of the other record in names and types.
In the latter case, record is a subtype of another record if they are the same record.

In the latter case, the type variable needs to carry a stable identifier, for example the module the record came from as well as a locally unique identifier (e.g. iota) for the definition.

This allows to carry these types across modules via `require` while maintaining the stable identity; for example:

```lua
-- module A
export record R { ... }

-- module B
local A = require(A)
export type R = A.R

-- module C
local A = require(A)
export type R = A.R

-- module D
local B = require(B)
local C = require(C)
-- B.R and C.R are the same type because the source of the definition is the same and comes from module A
```

In either case, the subtyping relationship between tables and records is structural and follows the is-a substitution principle. This is important because in code like this the inferred type is a table:

```lua
function f(p)
    return p.x + p.y
end
```

... and we'd like to be able to call `f` with a record as an argument. This also allows us to use table types as interfaces that records comply to, for example this would typecheck:

```lua
type Writer = { write: (Writer, string) -> () }

record Printer = {}

function Printer:write(s: string)
    print(s)
end

local w: Writer = Printer {}
```

> TODO: This draft RFC doesn't make the decision between nominal vs structural subtyping of two record types; this choice is going to be finalized when the RFC goes out of draft.

### Object modeling

With records, it becomes easy to model objects, which raises the question - do we need classes? Do we need traditional OOP features and if so, which ones?

This RFC is designed to provide a minimal foundation for modeling objects with associated methods, without imposing restrictions, or providing extensive features. In the spirit of Lua, we add the minimal viable data structure
with rigidly defined structure and do not do anything else.

Records can be used as an equivalent of "plain old data" structs: a single-line record definition is usable without the introduction of any methods, simply as a data container. The functions can be defined externally or as methods,
depending on the user preference.

Records don't provide a facility for implementation inheritance: adding fields or methods to a record requires defining a new record. This is something that is possible to implement in the future, by extending the syntax to be able to
provide the parent record when defining a new record shape, and requiring all fields to be specified. However, doing so is not only outside of the scope of this RFC, but also the author would like to note that implementation inheritance
is often considered an anti-pattern and composition or interface inheritance are preferred instead.

Records don't provide a facility for interface inheritance; however, existing support for table types along with subtyping rules allows records to be used when an "interface" table that defines methods is specified in the type signature.
At runtime, access to tables or fields is uniform and as such interface inheritance "just works". In the future we may consider adding syntax for enforcing the fact that the record R implements interface I, which could be helpful for typed code.

Records don't provide a facility for encapsulation: fields are readable and writeable. This is consistent with table fields; in the future, it would be possible to provide encapsulation as an option via extra attributes on fields, although
it's not clear if this is a worthwhile addition at this point.

Records don't provide a facility for computed properties: fields are used for data storage, and methods are used for function invocation. This can be changed in the future by allowing `__index`/`__newindex` invocation in cases when the field
is missing on a record, or by introducing special facility for property invocation - however, doing so is likely to carry a runtime cost as well as make it more difficult to reason about the side effects of the code so it's not clear if this is
a worthwhile addition at this point.

In short, records are the minimum viable mechanism for OOP: they provide a way to bind data and code without requiring it, they provide a way to think about interface inheritance via dynamic dispatch and table type annotations, and they provide
nothing else.

In the future we may consider extending records with more features but in the spirit of minimalism and considering that many successfull languages don't have a full OOP featureset and OOP isn't universally considered to be the right
way to model the world, we will be very careful in selecting features that we add to this data type.

### Ergonomics

Today it's possible to define objects using tables with metatables; this requires remembering a certain pattern that contains two magical lines, both relating to metatables:

```lua
local Point = {}
Point.__index = Point

function Point.new(x, y)
    return setmetatable({x = x, y = y}, Point)
end

function Point.__add(l, r)
    return Point.new(l.x + r.x, l.y + r.y)
end

function Point:sum()
    return self.x + self.y
end
```

This gets tricky when types are involved. The code specified above doesn't typecheck in strict mode; in particular, it doesn't contain a definition of the type Point.
It's tempting to fix it as follows:

```lua
type Point = { x: number, y: number }

local Point = {}
Point.__index = Point

function Point.new(x: number, y: number): Point
    return setmetatable({x = x, y = y}, Point)
end

function Point.__add(l: Point, r: Point): Point
    return Point.new(l.x + r.x, l.y + r.y)
end

function Point:sum(): number
    return self.x + self.y
end
```

However, this still doesn't typecheck - the setmetatable call returns a type that can't be converted to Point, and sum method doesn't know that self is a Point.
Furthermore, because of how we typecheck methods, the inferred type for the `Point` table grows exponentially with the number of method interactions in certain cases,
which results in very long type checking if limits are disabled or "code too complex" errors.

We have plans to improve this in the future, and `:sum` can be fixed by switching to an explicit `self` although that then runs the risk of issuing confusing errors around
use of `.` vs `:` in certain type error scenarios.

Finally, note that the `Point` type here is incorrect as it doesn't contain the definitions of any methods so it's not useful externally. It's possible to use `typeof` like this:

```lua
type Point = typeof(Point.new(0, 0))
```

... but this doesn't always work due to complex issues with toposort in real-world code, is not intuitive, requires a specific non-intuitive order of declarations, makes it hard
to specify the exact shape of the fields, and is even more difficult for generic code. For this simple example it does work, and along with `self` tweak this results in the following
type-safe code:

```lua
local Point = {}
Point.__index = Point

function Point.new(x: number, y: number): Point
    return setmetatable({x = x, y = y}, Point)
end

type Point = typeof(Point.new(0, 0))

function Point.__add(l: Point, r: Point): Point
    return Point.new(l.x + r.x, l.y + r.y)
end

function Point.sum(self: Point): number
    return self.x + self.y
end
```

Records solve all of these issues without requiring complex workarounds and result in code that is easier to read and reason about, and easier to teach:

```lua
record Point = { x: number, y: number }

function Point.new(x: number, y: number): Point
    return Point {x = x, y = y}
end

function Point.__add(l: Point, r: Point): Point
    return Point.new(l.x + r.x, l.y + r.y)
end

function Point:sum(): number
    return self.x + self.y
end
```

This code is type-safe in strict mode under this proposal. It's also valuable to point out that this code was produced by taking the table-driven code, removing needless lines
and replacing the `setmetatable` call with record constructor. The ease of conversion makes the author optimistic that the feature will be loved and adopted and won't introduce
extra friction or confusion. This would also be a reason to prefer the syntactic variant A.

## Drawbacks

Adding a new data type that is cross-cutting (across syntax, semantics/compiler, semantics/type checking, and runtime) results in added complexity.

The rigidity of records may make some applications hesitate to adopt them; e.g. you can't simply add a new field at a random point in the program, which some would
argue makes the language less dynamic and therefore less convenient.

By only supporting efficient representation of records, rather than all tables, we are only providing an optimization for new (or modified) code. JavaScript runtimes,
in comparison, support shape optimizations for all objects, although that optimization is hidden and as such isn't always reliable and can result in performance cliffs in
certain cases.

Not enforcing type compatibility for typed records at runtime may make it difficult for us to optimize record storage more by removing the type tags (which could
make record objects ~2x more efficient in some cases).

## Alternatives

Instead of using explicit record types, we can make the VM recognize shapes of objects automatically, just like JavaScript implementations do. This requires a
substantial amount of complicated machinery and heuristics, and likely can't be as efficient as records in the long run, but it can result in close efficiency
without any changes to existing programs. This, however, leaves the problem of establishing complex relationships between object shape and method on the type level
which requires heuristics with table-based OOP.

Instead of using explicit record types, we can lower a similar ergonomic syntax onto a table-based runtime along with similar type-level rules for how to bind
table methods to the table type. For example, we can still use the same `record X = { fields }` syntax that defines the type and a metatable simultaneously. If we setup
the table shape with `__call` metamethod automatically, we'll get the same syntax for record construction as well - here's what the lowering could look like (generated code):

```
local Point = {}
Point.__index = Point
Point.__call = function(tab) return setmetatable(tab, Point) end

type Point = { x: number, y: number, @metatable: typeof(Point) }
```

The rest would be handled by the type checker, including implicit `self` typing for methods declared on the table. In this alternative we'd need to either ignore the efficiency
gains, or rely on the complex runtime machinery that automatically recognizes shapes via a combination of compiler analysis and runtime instrumentation.

Instead of using record types that have minimal featureset, we could implement classes that have a more feature-rich OOP semantics, with inheritance, first class
properties, and access control. This would better map to other high level languages like TypeScript/Python, but would make the language and runtime more complicated.

Instead of allowing records to have metatables, we could have separate dedicated storage for methods and come up with a new scheme for operator overloading. This
would better map to other high level languages like C++ or C#, but would make the language less consistent.

Instead of defining records separately from arrays, we could define interactions between records-stored-inside-arrays (achieving single-allocation arrays of compound objects)
and arrays-stored-inside-records (making it possible to store a fixed size array in a record). Both of these really aren't compatible with TValue storage and result in
higher implementation effort.
