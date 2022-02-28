# Metatable keyword in type annotations

## Summary

Introduce a new contextual keyword `metatable` to define the type of a metatable for a table.

## Motivation

Today, the only way to attach a metatable to a table, one has to write `typeof(setmetatable(t, mt))` in type annotation context. While supported, it's not the most obvious path for everyone and leaves a pretty critical gap between the syntax and the type checker for an important part of Lua.

## Design

We already have the concept of metatables in the type inference engine, so this RFC will not get into the subtyping relationship or anything of the sort. The scope of this RFC is specifically to expose syntax to produce a type variable whose table has this metatable, and no more than that.

We want to introduce a new contextual keyword `metatable` where we can write `metatable <type>` inside table type annotations. It can show up anywhere in a table type next to properties or indexers. Just like indexers, only one metatable is permitted per table and it will be enforced at parse time.

By being able to write that a table has some metatable, we would be able to say that our data type has overloaded some operators. For example, our `Vec3` where the `+` operator takes two `Vec3` and produces a new `Vec3`:

```
type Vec3 = {
    x: number,
    y: number,
    z: number,
    metatable {
        __add: (self: Vec3, other: Vec3) -> Vec3
    }
}
```

Furthermore, the metatable syntax can also help with writing the type definitions for idiomatic OO code.

```
type Person = {
    name: string,
    metatable PersonImpl,
}

type PersonImpl = {
    __index: PersonImpl,
    new: (name: string) -> Person,
    say: (self: Person, message: string) -> ()
}

local Person = {} :: PersonImpl
Person.__index = Person

function Person.new(name)
    local person = {}
    person.name = name
    return setmetatable(person, Person)
end

function Person:say(message)
    print(self.name .. ": " .. message)
end
```

The reason why two type synonyms are required here is because every instance created by `Person.new` is of type `Person`, but the table `Person` is what implements the methods for each instance of `Person`. The `self` in each methods of the table `Person` is _not_ `PersonImpl` because usually we want to call the methods of `PersonImpl` where a `Person` is passed as `self`.

We would also be able to define the `getmetatable` function without using C++.

```
declare function getmetatable<T>(tab: {metatable {__metatable: T}}): T
declare function getmetatable<MT>(tab: {metatable MT}): MT
declare function getmetatable(tab: any): nil
```

## Drawbacks

The syntax is unfortunately very identical to properties, where the only difference between them is a single character, `:`. Observe: `{metatable T}` vs `{metatable: T}` where the former is a table with the metatable of type T, and the latter is a table with one property `metatable` of type `T`.

## Alternatives

A few other alternative designs have been proposed in the past, but ultimately were decided against for various reasons.

### 1: `setmetatable()`

One option is to copy the design of `typeof` for this use case: `setmetatable(<type>, <type>)`. However, we decided against it because it is inconsistent with `typeof` where it uses `()` to access the value namespace, whereas `setmetatable` would use `()` to access the type namespace.

Another issue is in the name itself, *set*metatable, which reads like it will mutate the type variable to have the metatable. There are cases where it is incorrect to mutate the type variable, so we would also have to introduce `withmetatable(<type>, <type>)` along with `setmetatable(<type>, <type>)`.

This option also does not allow a way to return the metatable for any given type variable. The only way to do so is to add _yet another_ syntax, `getmetatable(<type>)`.

That means we need 3 new syntax in total to support different use cases:

1. `setmetatable(T, MT)` where it is a side-effecting function that mutably adds the metatable `MT` to `T`.
2. `withmetatable(T, MT)` where it is a pure function that returns a copy of `T` with `MT` attached.
3. `getmetatable(T)` where it returns the metatable type of `T` if it exists, the type of `__metatable` field if it exists, or `nil` type otherwise.

### 2: `setmetatable<>`

Another option is to define a `setmetatable<T, MT>` type function from the C++ side, which would indeed bridge the gap, but all type functions are pure, so the name `setmetatable` is thus inaccurate. It is more accurately called `withmetatable`. If we were to opt for this option, we would rather name it `withmetatable`.

It still does not grant us any syntax to return the metatable of the type, or `nil` otherwise. We would need to define a `getmetatable<T>` type function from C++ again, but even that would be inaccurate in some edge cases (metatable has `__metatable` or is missing/not a table, so returns `nil`) and requires either conditional types or making this an instrinsic type.

This option would mean `withmetatable<>` and `getmetatable<>` are the first exported types in Luau's prelude, which is not something that we want to do at this time.
