# Attributes

## Summary

Syntax that allows functions/decorators to declaratively wrap and annotate tables, functions, and key/value pairs with rich metatables.

## Motivation

Consider the following object-oriented code:
```lua
local function CreatePerson(o)
    o = o or {}
    o.Age = o.Age or 0;
    o.Name = o.Name or "John Doe"
    return setmetatable(o, Person)
end

local Person = {}

function Person:GetAge()
    return self.Age
end
```
The foundation of this code is the highly imperative constructor.
The goal of attributes is to replace this implicit boilerplate with user-provided attributes.
An example of usage of the attributes regarding the above example might be:

```lua
@Constructor(Person)
local function CreatePerson(@Default({}) o)
    o.Age = o.Age or 0
    o.Name = o.Name or "John Doe"
    return o;
end
--  local Person = {}
--  ...
```
The logic for handling metatables and default values is shoved away in attributes, letting users reuse code and simplify their logic codebases.
The expected outcome is a tool to enable all corners of the Luau ecosystem to benefit from higher code reuse, simplicty, and saliency.

A more complicated example might be accelerating binary serialization using `string.pack`, and using attributes to define types and fields to be serialized. Maybe the library could also support JSON, or messagepack. Regardless of implementation, attributes would enable seamless declarative syntax.

```ts
@serializable("string.pack", "json")
data_packet = {
    @json
    @pack("int32")
    id = 0;
    
    @json
    @pack{"string", MaxLength = 128}
    name = "";

    @json
    @pack("float")
    health = 0;

    @json
    @pack("array", {"string", MaxLength = 128})
    friends = {};

    @json_ignore
    @pack("int32")
    crc32 = 0;
}
```

As another example, this error-catching scenario could be changed to use declarative attributes instead:
```lua
do
    local function Implementation(arg)
        assert(arg ~= 0, "Thing is zero!!!")
    end

    API.Implementation = Catch(Implementation, ErrorHandler)
end
--  Change to...
@Catch(ErrorHandler)
function API.Implementation(arg)
    assert(arg ~= 0, "Thing is zero!!!")
end
```

## Design

I will use the following vocabulary in this proposal:
 - **Targets** are the Luau object that an attribute modifies, wraps, decorates, etc. Also called the "relevant object".
 - **Attributes** are the function that performs the decoration,
 - **Description** is the description of the target provided by Luau to the attribute.
 - **Arguments** are the arguments passed to the attribute and are used for further configuration.

Attributes run exactly once each time a copy of it's target is being instantiated.
For functions, this will be exactly once per load into the VM. For tables, this will be every time the code that creates the table is run.

Attributes should be allowed to have a benign, optional semicolon after each attribute. Each target may have multiple attributes.

```lua
function Attribute(description, argument, ...)

end

@Attribute(argument)
function Target(@Attribute(argument) ...)

    @Attribute(argument)
    local target = {
        @Attribute(argument)
        ["Target"] = "Target!"
    }
end
```

A description is defined as follows:
```lua
type Description<T> = {
    --  Readonly
    --  The location that this attribute has been placed
    type: string,
    --  Read/Write
    --  The key that this object will be assigned to under "parent".
    name: string,
    --  Readonly
    --  The object that this object will be assigned to, under "key"
    parent: any,
    --  Read/Write
    --  The object in question (target)
    value: T,
}
```
| Type     | Example                                | `name` Source | `parent` Source | `value` Source   | Impact                                                                                                                                            |
| -------- | -------------------------------------- | ------------- | --------------- | ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| Function | `[local] function [parent(:/.)]name()` | "name"        | "parent" or nil | function ref     | Replace `value` to change function implementation                                                                                                 |
| Argument | `function(argument)`                   | "argument"    | "function"      | Passthrough      | Replace "passthrough", a function which simply returns all arguments, with a wrapper to change parameters before they are passed to the function. |
| Table    | `[local] name = { ... fields ... }`    | "name"        | nil             | `{.. fields ..}` | Assign metatable, modify fields, or change table altogether                                                                                       |
| Field    | `parent = {[name] = value, ...}`       | "name"        | "parent"        | assignment value | Change value, or assign metatable to parent to modify meaning                                                                                     |

Under the hood, attributes should be processed and placed **inside** the function body by the compiler. The transpiler should also perform this inlining automatically. For example:
```lua
@Func
local function Example(@Arg fancyArg)

end
```
...Should be *approximately* compiled and/or transpiled to:
```lua
--  @Arg
local __argDescription = {
    type = "argument",
    name = "fancyArg",
    parent = Example,
    value = GetPassthroughFunc()
}
Arg(__argDescription)

--  @Func
local __description = {
    type = "function",
    name = "example",
    --  Being assigned to a scope, not a table.
    --  Nil.
    parent = nil,
    value = Example
}
Func(__description)

local function Example(arg1)
    arg1 = __argDescription.Value(arg1)
    __description.value(arg1)
end
```

The following attributes (as long as others at implementor's discretion) should be reserved and cannot be used, throwing a type error if usage is attempted. This is to allow them to be used by the Luau type checker in the future:
 - `luau`
 - `compatibility`
 - `version`
 - `type`
 - `check`
 - `disable`
 - `usage`
 - `optional`
 - `required`
 - `condition`
 - `pragma`
 - `must_use`
 - `inline`
 - `unused`
 - `view_as`
 - `readonly`

It is highly recommended that the implementer adds attribute support to `pcall` for functions.

Psuedo-grammar for an attribute:
```
FUNCTION_NAME = SYMBOL + optional( one_of(".", ":" ) + FUNCTION_NAME);
ATTRIBUTE_DEFINITION = "@" + FUNCTION_NAME + optional( ARGUMENTS ) + optional(";")
```

Execution order for attributes during initialization should also not be taken lightly, as attributes may want to communicate with each other via back-channels such as metatables or a hidden upvalue.
1. Function Argument Attributes
2. Function Attributes
3. Table Field Attribtes
4. Table Attributes

When multiple attributes are specified for a target, they should be executed in order of specification, from top to bottom/left to right/first to last.

## Drawbacks

While the performance impact should be minimal, excessive or nested usage of attributes could exponentially grow code size (and execution complexity) and introduce hidden costs to critical & hot path code.

Additionally, attributes could severely harm the debuggability of Luau code without potentially extensive debugger changes.

Overall, this would be a major change to the compiler (and potentially the interpreter). While it could have a positive impact on the Luau ecosystem, the added complexity may not be worth it.

## Alternatives

Another lighter implementation of attribute/annotations used by Java and C# is to simply have attributes act as metadata (In Luau's case, this could be as simple as writing attributes to a dedicated metatable slot). 

However, this has the downside of requiring additional boilerplate logic to fetch and make use of the attributes. It would limit the scope of attributes from acting as powerful wrappers to simply providing information for future reflection.