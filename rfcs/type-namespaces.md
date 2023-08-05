# Type Namespaces

## Summary

Add user syntax to define namespaces for type declarations.

## Motivation

When working with multiple modules in a project, it is common for users to use a separate module solely for containing type declarations to avoid having cyclic requires in the typechecker. One of these modules may look like this:

```lua
export type ShapeType = "Triangle" | "Square" | "Circle"

export type ObjectParams = {
	Name: string,
	Shape: ShapeType,
	Position: Vector3
}

return nil
```

An added benefit of this pattern is that, when the above module is required, a namespace is created for the exported types in the module's name:

```lua
local Types = require("Types")

local MyShape: Types.ShapeType = "Circle"

local Object = {
	Name = "MyObject",
	Shape = MyShape,
	Position = Vector3.zero
} :: Types.ObjectParams
```
This RFC aims to streamline defining type namespaces like the above without having to use modules as well as allowing for multi-dimensional namespaces to help organize large projects (i.e., `Types.Shape.ObjectParams`).

## Design

Namespaces could be added to any type declaration via a dot to discern between space name and type name:

```lua
type Space.Name = number

local Foo: Space.Name = 1
```

This syntax could also allow for nested namespaces as well as generated namespaces for exported types:

```lua
export type Types.Space.Name<A, B> = A | B
```

## Drawbacks

This syntax design may introduce repetition for multiple types as it is by declaration-basis. For example:

```lua
export type Space.Foo = number | string
export type Space.Bar = "Hello" | "World"
export type Space.Baz = boolean
```

Mistypes could confuse users as they create a new namespace for the type:

```lua
export type Space.A = number
export type Spaze.B = string
```

Nested spaces could also confuse users since namespaces could be mingled with actual types:

```lua
local Foo: Space.Foo = 1
local Bar: Space.Bar.Foo = true
```

## Alternatives

Using the `in` keyword instead:

```lua
type Name in Space = number
```

Having overarching syntax for namespaces:

```lua
export Space
	type foo = number,
	type bar = string
```
