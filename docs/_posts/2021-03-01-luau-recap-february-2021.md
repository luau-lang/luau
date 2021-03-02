---
layout: single
title:  "Luau Recap: February 2021"
---

Luau is our new language that you can read more about at [https://roblox.github.io/luau](https://roblox.github.io/luau). It's been a busy few months in Luau!

[Cross-posted to the [Roblox Developer Forum](https://devforum.roblox.com/t/luau-recap-february-2021/).]

## Infallible parser

Traditional compilers have focused on tasks that can be performed on complete programs, such as type-checking, static analysis and code generation. This is all good, but most programs under development are incomplete! They may have holes, statements that will be filled in later, and lines that are in the middle of being edited. If we'd like to provide support for developers while they are writing code, we need to provide tools for incomplete programs as well as complete ones.

The first step in this is an *infallible* parser, that always returns an Abstract Syntax Tree, no matter what input it is given. If the program is syntactically incorrect, there will also be some syntax errors, but the parser keeps going and tries to recover from those errors, rather than just giving up.

The Luau parser now recovers from errors, which means, for example, we can give hints about programs in an IDE.

![A type error after a syntax error]({{ site.url }}{{ site.baseurl }}/assets/images/type-error-after-syntax-error.png)

## Type assertions

The Luau type checker can't know everything about your code, and sometimes it will produce type errors even when you know the code is correct. For example, sometimes the type checker can't work out the intended types, and gives a message such as "Unknown type used... consider adding a type annotation".

!["Consider adding a type annotation"]({{ site.url }}{{ site.baseurl }}/assets/images/type-annotation-needed.png)

Previously the only way to add an annotation was to put it on the *declaration* of the variable, but now you can put it on the *use* too.  A use of variable `x` at type `T` can be written `x :: T`. For example the type `any` can be used almost anywhere, so a common usage of type assertions is to switch off the type system by writing `x :: any`.

!["A type assertion y:any"]({{ site.url }}{{ site.baseurl }}/assets/images/type-annotation-provided.png)

## Performance improvements

We've made quite a few performance improvements to the Luau bytecode interpreter.

 * Function calls are now faster in the common case of one- or two-argument functions.
 * Some built-in operations such as equality checking and modulo arithmetic are now faster.

## Coming soon...

* _Generic function types_ will soon be allowed!
```
function id<a>(x: a): a
    return x
end
```

* _Typed variadics_ will soon allow types to be given to functions with varying numbers of arguments!
```
function sum(...: number): number
    local result = 0
    for i,v in ipairs({...}) do
        result += v
    end
    return result
end
```

And there will be more!
