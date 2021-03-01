---
layout: single
title:  "Luau Type Checking Beta"
---

Hello!

We’ve been quietly working on building a type checker for Lua for quite some time now. It is now far enough along that we’d really like to hear what you think about it.

I am very happy to offer a beta test into the second half of the Luau effort.

[Originally posted on the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-type-checking-beta/).]

## Beta Test

First, a word of caution: In this test, we are changing the syntax of Lua. We are pretty sure that we’ve mostly gotten things right, but part of the reason we’re calling this a beta is that, if we learn that we’ve made a mistake, we’re going to go back and fix it even if it breaks compatibility.

Please try it out and tell us what you think, but be aware that this is not necessarily our final form.  🙂

Beta testers can try it out by enabling the “Enable New Lua Script Analysis” beta feature in Roblox Studio.

## Overview

Luau is an ahead-of-time typechecking system that sits atop ordinary Lua code. It does not (yet) feed into the runtime system; it behaves like a super powerful lint tool to help you find bugs in your code quickly.

It is also what we call a gradual type system. This means that you can choose to add type annotations in some parts of your code but not others.

## Two Modes

Luau runs in one of two modes: strict, and nonstrict.

### Nonstrict Mode

Nonstrict mode is intended to be as helpful as possible for programs that are written without type annotations. We want to report whatever we can without reporting an error in reasonable Lua code.

 * If a local variable does not have a type annotation and it is not initially assigned a table, its type is any
 * Unannotated function parameters have type any
 * We do not check the number of values returned by a function
 * Passing too few or too many arguments to a function is ok
 
### Strict Mode

Strict mode is expected to be more useful for more complex programs, but as a side effect, programs may need a bit of adjustment to pass without any errors.

 * The types of local variables, function parameters, and return types are deduced from how they are used
 * Errors are produced if a function returns an inconsistent number of parameters, or if it is passed the wrong number of arguments
 
Strict mode is not enabled by default. To turn it on, you need to add a special comment to the top of your source file.
```
--!strict
```

## New syntax

You can write type annotations in 5 places:

 * After a local variable
 * After a function parameter
 * After a function declaration (to declare the function’s return type)
 * In a type alias, and
 * After an expression using the new as keyword.

```
local foo: number = 55

function is_empty(param: string) => boolean
    return 0 == param:len()
end

type Point = {x: number, y: number}

local baz = quux as number
```

## Type syntax
### Primitive types

`nil`, `number`, `string`, and `boolean`

### any
The special type any signifies that Luau shouldn’t try to track the type at all. You can do anything with an any.

### Tables
Table types are surrounded by curly braces. Within the braces, you write a list of name: type pairs:
```
type Point = {x: number, y: number}
```
Table types can also have indexers. This is how you describe a table that is used like a hash table or an array.
```
type StringArray = {[number]: string}

type StringNumberMap = {[string]: number}
```

### Functions

Function types use a `=>` to separate the argument types from the return types.
```
type Callback = (string) => number
```
If a function returns more than one value, put parens around them all.
```
type MyFunction = (string) => (boolean, number)
```

### Unions

You can use a `|` symbol to indicate an “or” combination between two types. Use this when a value can have different types as the program runs.
```
function ordinals(limit)
    local i = 0
    return function() => number | nil
        if i < limit then
            local t = i
            i = i + 1
            return t
        else
            return nil
        end
    end
end
```

### Options

It’s pretty commonplace to have optional data, so there is extra syntax for describing a union between a type and `nil`. Just put a `?` on the end. Function arguments that can be `nil` are understood to be optional.
```
function foo(x: number, y: string?) end

foo(5, 'five') -- ok
foo(5) -- ok
foo(5, 4) -- not ok
```

### Type Inference

If you don’t write a type annotation, Luau will try to figure out what it is.
```
--!strict
local Counter = {count=0}

function Counter:incr()
    self.count = 1
    return self.count
end 

print(Counter:incr()) -- ok
print(Counter.incr()) -- Error!
print(Counter.amount) -- Error!
```

## Future Plans

This is just the first step!

We’re excited about a whole bunch of stuff:

 * Nonstrict mode is way more permissive than we’d like
 * Generics!
 * Editor integration