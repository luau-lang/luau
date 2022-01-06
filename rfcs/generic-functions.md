# Generic functions

**Status**: Implemented

## Summary

Extend the syntax and semantics of functions to support explicit generic functions, which can bind type parameters as well as data parameters.

## Motivation

Currently Luau allows generic functions to be inferred but not given explicit type annotations. For example

```lua
function id(x) return x end
local x: string = id("hi")
local y: number = id(37)
```

is fine, but there is no way for a user to write the type of `id`.

## Design

Allow functions to take type parameters as well as function parameters, similar to Java/Typescript/...

```lua
function id<a>(x : a) : a return x end
```

Functions may also take generic type pack arguments for varargs, for instance:

```lua
function compose<a...>(... : a...) -> (a...) return ... end
```

Generic type and type pack parameters can also be used in function types, for instance:

```lua
local id: <a>(a)->a = function(x) return x end
```

This change is *not* only syntax, as explicit type parameters need to be part of the semantics of types. For example, we can define a generic identity function

```lua
local function id(x) return x end
local x: string = id("hi")
local y: number = id(37)
type Id = typeof(id)
```

and two functions 

```lua
function f()
  return id
end
function g()
  local y
  function oh(x)
    if not(y) then y = x end
    return y
  end
  return oh
end
```

The types of these functions are

```lua
  f : () -> <a>(a) -> a
  g : <a>() -> (a) -> a
```

so this is okay:

```lua
  local i: Id = f()
  local x: string = i("hi")
  local y: number = i(37)
```

but this is not:

```lua
  -- This assignment shouldn't typecheck!
  local i: Id = g()
  local x: string = i("hi")
  -- This is unsound, since it assigns a string to a variable of type number
  local y: number = i(37)
```

Currently, Luau does not have explicit type binders, so `f` and `g` have the same type. We propose making type binders part of the semantics of types as well as their syntax (so `f` and `g` have different types, and the unsound example does not typecheck).

We propose supporting type parameters which can be instantiated with any type (jargon: Rank-N Types) but not type functions (jargon: Higher Kinded Types) or types with constraints (jargon: F-bounded polymorphism).

## Turbofish

Note that this RFC proposes a syntax for adding generic parameters to functions, but it does *not* propose syntax for adding generic arguments to function call site. For example, for `id` function you *can* write:

```lua
 -- generic type gets inferred as a number in all these cases
local x = id(4)
local x = id(y) :: number
local x: number = id(y)
```

but you can *not* write `id<number>(y)`.

This syntax is difficult to parse as it's ambiguous wrt grammar for comparison, and disambiguating it requires being able to parse types in expression context which makes parsing slow and complicated. It's also worth noting that today there are programs with this syntax that are grammatically correct (eg `id<string>('4')` parses as "compare variable `id` to variable `string`, and compare the result to string '4'"). The specific example with a single argument will always fail at runtime because booleans can't be compared with relational operators, but multi-argument cases such as `print(foo<number, string>(4))` can execute without errors in certain cases.

Note that in many cases the types can be inferred, whether through function arguments (`id(4)`) or through expected return type (`id(y) :: number`). It's also often possible to cast the function object to a given type, even though that can be unwieldy (`(id :: (number)->number)(y)`). Some languages don't have a way to specify the types at call site either, Swift being a prominent example. Thus it's not a given we need this feature in Luau.

If we ever want to implement this though, we can use a solution inspired by Rust's turbofish and require an extra token before `<`. Rust uses `::<` but that doesn't work in Luau because as part of this RFC, `id::<a>(a)->a` is a valid, if redundant, type ascription, so we need to choose a different prefix.

The following two variants are grammatically unambiguous in expression context in Luau, and are a better parallel for Rust's turbofish (in Rust, `::` is more similar to Luau's `:` or `.` than `::`, which in Rust is called `as`):

```lua
foo:<number, string>() -- require : before <; this is only valid in Luau in variable declaration context, so it's safe to use in expression context
foo.<number, string>() -- require . before <; this is currently never valid in Luau
```

This RFC doesn't propose using either of these options, but notes that either one of these options is possible to specify & implement in the future if we so desire.

## Drawbacks

This is a breaking change, in that examples like the unsound program above will no longer typecheck.

Types become more complex, so harder for programmers to reason about, and adding to their space usage. This is particularly noticeable anywhere the typechecker has exponential blowup, since small increases in type size can result in large increases in space or time usage.

Not having higher-kinded types stops some examples which are parameterized on container types, for example:

```lua
  function g<c>(f : <a>(a) -> c<a>) : <b>(b) -> c<c<b>>
    return function(x) return f(f(x)) end
  end
```

Not having bounded types stops some examples like giving a type to the function that sums an non-empty array:

```lua
  function sum(xs)
    local result = x[0]
    for i=1,#xs
      result += x[i]
    end
    return result
  end
```

## Alternatives

We did originally consider Rank-1 types, but the problem is that's not backward-compatible, as DataBrain pointed out in the [Dev Forum](https://devforum.roblox.com/t/luau-recap-march-2021/1141387/29), since `typeof` allows users to construct generic types even without syntax for them. Rank-1 types give a false positive type error in this case, which comes from deployed code.

We could introduce syntax for generic types without changing the semantics, but then there'd be a gap between the syntax (where the types `() -> <a>(a) -> a` and `<a>() -> (a) -> a` are different) and the semantics (where they are not). As noted above, this isn't sound.

Rather than using Rank-N types, we could use SML-style polymorphism, but this would need something like the [value restriction](http://users.cis.fiu.edu/~smithg/cop4555/valrestr.html) to be sound.
