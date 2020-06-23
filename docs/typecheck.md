# Type checking

Luau supports a gradual type system through the use of type annotations and type inference.

## Type inference modes

There are three modes currently available. They must be annotated on the top few lines among the comments.

* `--!nocheck`,
* `--!nonstrict` (default), and
* `--!strict`

`nocheck` mode will simply not start the type inference engine whatsoever.

As for the other two, they are largely similar but with one important difference: in nonstrict mode, we infer `any` for most of the types if we couldn't figure it out early enough. This means that given this snippet:

```lua
local foo = 1
```

We can infer `foo` to be of type `number`, whereas the `foo` in the snippet below is inferred `any`:

```lua
local foo
foo = 1
```

However, in strict mode, the second snippet would be able to infer `number` for `foo` still.

## Unknown symbols

You may see this error when using custom globals, and that's by design even in nonstrict mode.

Consider how often you're likely to assign a new value to a local variable. What if you accidentally misspelled it? Oops, it's now assigned globally and your local variable is still using the old value.

```lua
local someLocal = 1

soeLocal = 2 -- the bug

print(someLocal)
```

Because of this, Luau type checker currently emits an error whenever a non-function global is used; use local variables instead.

## Structural type system

Luau's type system is structural by default, which is to say that we inspect the shape of two tables to see if they are similar enough. This was the obvious choice because Lua 5.1 is inherently structural.

```lua
type A = {x: number, y: number, z: number?}
type B = {x: number, y: number, z: number}

local a1: A = {x = 1, y = 2}        -- ok
local b1: B = {x = 1, y = 2, z = 3} -- ok

local a2: A = b1 -- ok
local b2: B = a1 -- not ok
```

## Primitive types

Lua VM supports 8 primitive types: `nil`, `string`, `number`, `boolean`, `table`, `function`, `thread`, and `userdata`. Of these, `table` and `function` are not represented by name, but have their dedicated syntax as covered in this [syntax document](syntax.md), and `userdata` is represented by [concrete types](#Roblox-types); other types can be specified by their name.

Additionally, we also have `any` which is a special built-in type. It effectively disables all type checking, and thus should be used as last resort.

```lua
local s = "foo"
local n = 1
local b = true
local t = coroutine.running()

local a: any = 1
print(a.x) -- Type checker believes this to be ok, but crashes at runtime.
```

There's a special case where we intentionally avoid inferring `nil`. It's a good thing because it's never useful for a local variable to always be `nil`, thereby permitting you to assign things to it for Luau to infer that instead.

```lua
local a
local b = nil
```

## Function types

Let's start with something simple.

```lua
local function f(x) return x end

local a: number = f(1)     -- ok
local b: string = f("foo") -- ok
local c: string = f(true)  -- not ok
```

In strict mode, the inferred type of this function `f` is `<A>(A) -> A` (take a look at [generics](#generics)), whereas in nonstrict we infer `(any) -> any`. We know this is true because `f` can take anything and then return that. If we used `x` with another concrete type, then we would end up inferring that.

Similarly, we can infer the types of the parameters with ease. By passing a parameter into *anything* that also has a type, we are saying "this and that has the same type."

```lua
local function greetingsHelper(name: string)
    return "Hello, " .. name
end

local function greetings(name)
    return greetingsHelper(name)
end

print(greetings("Alexander")          -- ok
print(greetings({name = "Alexander"}) -- not ok
```

Another example is assigning a value to a local outside of the function: we know `x` and `y` are the same type when we assign `y` to `x`. By calling it, we assigned `x` the value of the argument we passed in. In doing so, we gave `x` a more concrete type, so now we know `x` is whatever type that got passed in.

```lua
local x
local function f(y) x = y end

f(1)     -- ok
f(2)     -- ok
f("foo") -- not ok
```

## Table types

From the type checker perspective, each table can be in one of three states. They are: `unsealed table`, `sealed table`, and `generic table`. This is intended to represent how the table's type is allowed to change.

### Unsealed tables

An unsealed table is a table whose properties could still be tacked on. This occurs when the table constructor literal had zero expressions. This is one way to accumulate knowledge of the shape of this table.

```lua
local t = {} -- {}
t.x = 1      -- {x: number}
t.y = 2      -- {x: number, y: number}
```

However, if this local were written as `local t: {} = {}`, it ends up sealing the table, so the two assignments henceforth will not be ok.

Furthermore, once we exit the scope where this unsealed table was created in, we seal it.

```lua
local function vec2(x, y)
    local t = {}
    t.x = x
    t.y = y
    return t
end

local v2 = vec2(1, 2)
v2.z = 3 -- not ok
```

### Sealed tables

A sealed table is a table that is now locked down. This occurs when the table constructor literal had 1 or more expression, or when the table type is spelt out explicitly via a type annotation.

```lua
local t = {x = 1} -- {x: number}
t.y = 2           -- not ok
```

### Generic tables

This typically occurs when the symbol does not have any annotated types or were not inferred anything concrete. In this case, when you index on a parameter, you're requesting that there is a table with a matching interface.

```lua
local function f(t)
    return t.x + t.y
           --^   --^ {x: _, y: _}
end

f({x = 1, y = 2})        -- ok
f({x = 1, y = 2, z = 3}) -- ok
f({x = 1})               -- not ok
```

## Table indexers

These are particularly useful for when your table is used similarly to an array.

```lua
local t = {"Hello", "world!"} -- {[number]: string}
print(table.concat(t, ", "))
```

## Generics

The type inference engine was built from the ground up to recognize generics. A generic is simply a type parameter in which another type could be slotted in. It's extremely useful because it allows the type inference engine to remember what the type actually is, unlike `any`.

```lua
type Array<T> = {[number]: T}

local strings: Array<string> = {"Hello", "world!"}
local numbers: Array<number> = {1, 2, 3, 4, 5, 6}
```

## Union types

A union type represents *one of* the types in this set. If you try to pass a union onto another thing that expects a *more specific* type, it will fail.

For example, what if this `string | number` was passed into something that expects `number`, but the passed in value was actually a `string`?

```lua
local stringOrNumber: string | number = "foo"

local onlyString: string = stringOrNumber -- not ok
local onlyNumber: number = stringOrNumber -- not ok
```

Note: it's impossible to be able to call a function if there are two or more function types in this union.

## Intersection types

An intersection type represents *all of* the types in this set. It's useful for two main things: to join multiple tables together, or to specify overloadable functions.

```lua
type XCoord = {x: number}
type YCoord = {y: number}
type ZCoord = {z: number}

type Vector2 = XCoord & YCoord
type Vector3 = XCoord & YCoord & ZCoord

local vec2: Vector2 = {x = 1, y = 2}        -- ok
local vec3: Vector3 = {x = 1, y = 2, z = 3} -- ok
```

```lua
type SimpleOverloadedFunction = (string) -> number & (number) -> string

local f: SimpleOverloadedFunction

local r1: number = f("foo") -- ok
local r2: number = f(12345) -- not ok
local r3: string = f("foo") -- not ok
local r4: string = f(12345) -- ok
```

Note: it's impossible to create an intersection type of some primitive types, e.g. `string & number`, or `string & boolean`, or other variations thereof.

Note: Luau still does not support user-defined overloaded functions. Some of Roblox and Lua 5.1 functions have different function signature, so inherently requires overloaded functions.

## Typing idiomatic OOP

One common pattern we see throughout Roblox is this OOP idiom. A downside with this pattern is that it does not automatically create a type binding for an instance of that class, so one has to write `type Account = typeof(Account.new("", 0))`.

```lua
local Account = {}
Account.__index = Account

function Account.new(name, balance)
    local self = {}
    self.name = name
    self.balance = balance

    return setmetatable(self, Account)
end

function Account:deposit(credit)
    self.balance += credit
end

function Account:withdraw(debit)
    self.balance -= debit
end

local account: Account = Account.new("Alexander", 500)
             --^^^^^^^ not ok, 'Account' does not exist
```

## Type refinements

When we check the type of a value, what we're doing is we're refining the type, hence "type refinement." Currently, the support for this is somewhat basic.

Using `type` comparison:
```lua
local stringOrNumber: string | number = "foo"

if type(x) == "string" then
    local onlyString: string = stringOrNumber -- ok
    local onlyNumber: number = stringOrNumber -- not ok
end

local onlyString: string = stringOrNumber -- not ok
local onlyNumber: number = stringOrNumber -- not ok
```

Using truthy test:
```lua
local maybeString: string? = nil

if maybeString then
    local onlyString: string = maybeString -- ok
end
```

And using `assert` will work with the above type guards:
```lua
local stringOrNumber: string | number = "foo"

assert(type(x) == "string")

local onlyString: string = stringOrNumber -- ok
local onlyNumber: number = stringOrNumber -- not ok
```

## Roblox types

Roblox supports a rich set of classes and data types, [documented here](https://developer.roblox.com/en-us/api-reference). All of them are readily available for the type checker to use by their name (e.g. `Part` or `RaycastResult`).

Additionally, we can automatically deduce what calls like `Instance.new` and `game:GetService` are supposed to return:

```lua
local part = Instance.new("Part")
local basePart: BasePart = part
```

Note that many of these types provide some properties and methods in both lowerCase and UpperCase; the lowerCase variants are deprecated, and the type system will ask you to use the UpperCase variants instead.

## Module interactions

Let's say that we have two modules, `Foo` and `Bar`. Luau will try to resolve the paths if it can find any `require` in any scripts. In this case, when you say `script.Parent.Bar`, Luau will resolve it as: relative to this script, go to my parent and get that script named Bar.

```lua
-- Module Foo
local Bar = require(script.Parent.Bar)

local baz1: Bar.Baz = 1     -- not ok
local baz2: Bar.Baz = "foo" -- ok

print(Bar.Quux)         -- ok
print(Bar.FakeProperty) -- not ok

Bar.NewProperty = true -- not ok
```

```lua
-- Module Bar
export type Baz = string

local module = {}

module.Quux = "Hello, world!"

return module
```

There are some caveats here though. The path must be resolvable statically, otherwise Luau cannot accurately type check it. There are three kinds of outcome for each require paths:

* Resolved - Luau was able to resolve this path statically,
* Module not found - The module at this path does not exist, and
* Unresolvable - This require path may resolve correctly at runtime
