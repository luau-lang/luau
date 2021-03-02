---
layout: single
title:  "Luau Type Checking Release"
---

10 months ago, we’ve started upon the journey of helping Roblox scripters write robust code by introducing [an early beta of type checking](https://devforum.roblox.com/t/luau-type-checking-release). We’ve received a lot of enthusiastic feedback and worked with the community on trying to make sure critical issues are addressed, usability is improved and the type system is ready for prime time.

Today I’m incredibly excited to announce that the first release of [Luau](https://roblox.github.io/luau/) type checking is officially released! Thanks a lot to @Apakovtac, @EthicalRobot, @fun_enthusiast, @machinamentum, @mrow_pizza and @zeuxcg!

[Originally posted on the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-type-checking-release/).]

## What is type checking?

When Luau code runs, every value has a certain type at runtime - a kind of value it stores. It could be a number, a string, a table, a Roblox Instance or one of many others. Thing is, some operations work on some types but don’t work on others!

Consider this:
```
local p = Instance.new("Part")
p.Positio = Vector3.new(1,2,3)
```
Is this code correct? No - there’s a typo. The way you get to find this typo is by running your code and eventually seeing an error message. Type checker tries to analyze your code before running, by assigning a type to each value based on what we know about how that value was produced, or based on the type you’ve explicitly told us using a new syntax extension, and can produce an error ahead of time:

!["Positio not found in class Part"]({{ site.url }}{{ site.baseurl }}/assets/images/luau-type-checking-release-screenshot.png)

This can require some effort up front, especially if you use strict mode, but it can save you valuable time in the future. It can be especially valuable if you have a large complex code base you need to maintain for years, as is the case with many top Roblox games.

## How do I use type checking?

A very important feature of Luau type checking you need to know about is that it has three modes:

 * `nocheck`, where we don’t type check the script in question.
 * `nonstrict`, where we type check the script but try to be lenient to allow commonly seen patterns even if they may violate type safety
 * `strict`, where we try to make sure that every single line of code you write is correct, and every value has a known type.
 
The modes can be selected per script by writing a comment at the top of the script that starts with `--!`, e.g. `--!strict`.

As of this release, the default mode is nocheck. This means by default you actually won’t see the type checking produce feedback on your code! We had to use nocheck by default because we aren’t fully ready to unleash nonstrict mode on unsuspecting users - we need to do a bit more work to make sure that most cases where we tell you that something is wrong are cases where yes, something is actually wrong.

However we highly encourage trying at least non-strict mode on your codebase. You can do this by opting into a different default via a Studio beta:

!["Studio option"]({{ site.url }}{{ site.baseurl }}/assets/images/luau-type-checking-release-studio-option.png)

This beta only changes the default mode. Another way to change the mode is to prepend a `--!` comment to the script - you can do this manually for now, but if anyone in the community wants to release a plugin that does it automatically on selected scripts (+ descendants), that would be swell!

If you really want your code to be rock solid, we recommend trying out strict mode. Strict mode will require you to use type annotations.

## What are type annotations and how do I use them?

Glad you asked! (please pretend you did) Type annotations are a way to tell the type checker what the type of a variable is. Consider this code in strict mode:
```
function add(x, y)
    return x + y
end
```
Is this code correct? Well, that depends. `add(2, 3)` will work just fine. `add(Vector3.new(1, 2, 3), Vector3.new(4, 5, 6))` will work as well. But `add({}, nil)` probably isn’t a good idea.

In strict mode, we will insist that the type checker knows the type of all variables, and you’ll need to help the type checker occasionally - by adding types after variable names separated by `:`:
```
function add(x: number, y: number)
    return x + y
end
```
If you want to tell the type checker “assume this value can be anything and I will take responsibility”, you can use `any` type which will permit any value of any type.

If you want to learn more about the type annotation syntax, you should read this [documentation on syntax](https://roblox.github.io/luau/syntax.html#type-annotations). We also have a somewhat more complete guide to type checking than this post can provide, that goes into more details on table types, OOP, Roblox classes and enums, interaction with require and other topics - [read it if you’re curious!](https://roblox.github.io/luau/typecheck.html).

## What happens when I get a type error?

One concept that’s very important to understand is that right now type errors do not influence whether the code will run or not.

If you have a type error, this means that our type checker thinks your code has a bug, or doesn’t have enough information to prove the code works fine. But if you really want to forge ahead and run the code - you should feel free to do so!

This means that you can gradually convert your code to strict mode by adding type annotations and have the code runnable at all times even if it has type errors.

This also means that it’s safe to publish scripts even if type checker is not fully happy with them - type issues won’t affect script behavior on server/client, they are only displayed in Studio.

## Do I have to re-learn Lua now?!?

This is a question we get often! The answer is “no”.

The way the type system is designed is that it’s completely optional, and you can use as many or as few types as you’d like in your code.

In non-strict mode, types are meant as a lightweight helper - if your code is likely wrong, we’re going to tell you about it, and it’s up to you on whether to fix the issue, or even disable the type checker on a given problematic file if you really don’t feel like dealing with this.

In strict mode, types are meant as a power user tool - they will require more time to develop your code, but they will give you a safety net, where changing code will be much less likely to trigger errors at runtime.

## Is there a performance difference?

Right now type annotations are ignored by our bytecode compiler; this means that performance of the code you write doesn’t actually depend on whether you use strict, nonstrict or nocheck modes or if you have type annotations.

This is likely going to change! We have plans for using the type information to generate better bytecode in certain cases, and types are going to be instrumental to just-in-time compilation, something that we’re going to invest time into next year as well.

Today, however, there’s no difference - type information is completely elided when the bytecode is built, so there is zero runtime impact one way or another.

## What is next for types?

This is the first full release of type checking, but it’s by far the last one. We have a lot more ground to cover. Here’s a few things that we’re excited about that will come next:

 * Making nonstrict mode better to the point where we can enable it as a default for all Roblox scripts

 * Adding several features to make strict mode more powerful/friendly, such as typed variadics, type ascription and better generics support

 * Improving type refinements for type/typeof and nil checks

 * Making it possible to view the type of a variable in Studio

 * Reworking autocomplete to use type information instead of the current system

If you have any feedback on the type system, please don’t hesitate to share it here or in dedicated bug report threads. We’re always happy to fix corner cases that we’ve missed, fix stability issues if they are discovered, improve documentation when it’s not clear or improve error messages when they are hard to understand.