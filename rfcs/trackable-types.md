# Trackable Types

## Summary

Add trackable types, types which when used in multiple places replicate their changes

## Motivation

Currently, the type of a variable cannot impact another variable's type. This becomes problematic when a number of variables are guaranteed to have a certain type when only one of them has the type.

Consider this scenario: In a 2D game, there are objects. These objects can optionally have text and if they have it, they also have a specified font.

```lua
type Font = "Font1" | "Font2" | "Font3"
type PlainObject = {X: number, Y: number, text: nil, font: nil}
type TextObject = {X: number, Y: number, text: string, font: Font}
type Object = PlainObject | TextObject
```

However, this is too lengthy. There is another point: What if instead of just having `text`, it had `data` which could have a different type depending on another value? Writing all possibilities in the entire table like that wouldn't be worth it when someone could just use the type `any` for it.

This gets more annoying when other places in which Luau is used don't provide precise types for the new things they add.

## Design

To make it feel like generics, angle brackets will be used for defining these types.

```lua
type Font = "Font1" | "Font2" | "Font3"
type Object = {X: number, Y: number, text: string | <nil_status = nil>, font: Font | nil_status}
```

The scope of these types should be the function in which they are used in. If they aren't defined in a function, then it is the global scope.

In this code, the type of `font` will never change the type of `text`, but the opposite is true. If `font` is guaranteed to be non-nil, text isn't. To make it replicate to text as well, `font` should have the type `<nil_status>` without initialization.

```lua
type Font = "Font1" | "Font2" | "Font3"
type Object = {X: number, Y: number, text: string | <nil_status = nil>, font: Font | <nil_status>}
```

For the case with `data`, a conditional could be added. This is not in the scope of this RFC, but here is a possible syntax for it:

```lua
type Object = {X: number, Y: number, typeof_data: <typeof_data = "text" | "image">, data: switch<typeof_data> {["text"]: string, ["image"]: {path: string, saturation: number}}}
```

In that example, `switch<T> table_type` is like a switch statement present in many languages, and checks if two types are the same.

## Drawbacks

This would require the addition of an empty type that changes nothing in union types when tracking a type used in a union type, which could increase complexity.

Assigning to things with trackable types could be difficult to typecheck. When trackable types are found, it could make sense to only allow multiple assignment to them.

Using `=` could be difficult for humans to parse when variables are initialized:

```lua
local number = 123
local var: <trackable = number> = 1
```

## Alternatives

The behavior of `typeof` could be changed and conditionals could be added.

When it comes to syntax, it could make sense to allow defining these trackable types when defining a generic function like this:

```lua
function f<type = nil>() --[[code]] end
```

This doesn't work when writing code outside a function. Also, this forces everything to be tracked - if two variables have types related to `type`, only one of them changing would be sufficient to change the type of the other variable. All of them are tracked; whereas with the suggested syntax, it is possible to track just one of them.

Another syntax is to use `:` instead of `=`. Another one is to use intersection syntax to initialize, like this:

```lua
<T> & number
```
