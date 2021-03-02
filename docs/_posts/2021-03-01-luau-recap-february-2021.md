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

## Typechecking improvements

We've made various improvements to the Luau typechecker:

 * We allow duplicate function definitions in non-strict mode.
 * Better typechecking of `and`, `(f or g)()`, arrays with properties, and `string:format()`.
 * Improved typechecking of infinite loops.
 * Better error reporting for function type mismatch, type aliases and cyclic types.

## Performance improvements

We are continuing to work on optimizing our VM and libraries to make sure idiomatic code keeps improving in performance. Most of these changes are motivated by our benchmark suite; while some improvements may seem small and insignificant, over time these compound and allow us to reach excellent performance.

 * Table key assignments as well as global assignments have been optimized to play nicer with modern CPUs, yielding ~2% speedup in some benchmarks
 * Luau function calls are now ~3% faster in most cases; we also have more call optimizations coming up next month!
 * Modulo operation (%) is now a bit faster on Windows, resulting in ~2% performance improvement on some benchmarks

!["Benchmark vs Lua 5.3"]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-february-2021-benchmark.png)

## Debugger improvements

Our Luau VM implementation is focused on performance and provides a different API for implementation of debugger tools. But it does have its caveats and one of them was inability to debug coroutines (breakpoints/stepping).

The good news is that we have lifted that limitation and coroutines can now be debugged just like any regular function. This can especially help people who use Promise libraries that rely on coroutines internally.

![Debugging a coroutine]({{ site.url }}{{ site.baseurl }}/assets/images/luau-recap-february-2021-debugger.png)

## Library changes

`table` library now has a new method, `clear`, that removes all keys from the table but keeps the internal table capacity. When working with large arrays, this can be more efficient than assigning a table to `{}` - the performance gains are similar to that of using `table.create` instead of `{}` *when you expect the number of elements to stay more or less the same*. Note that large empty tables still take memory and are a bit slower for garbage collector to process, so use this with caution.

In addition to that we found a small bug in `string.char` implementation that allowed creating strings from out-of-range character codes (e.g. `string.char(2000)`); the problem has been fixed and these calls now correctly generate an error.

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
