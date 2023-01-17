# Function annotations

## Summary

Reserve a syntax for being able to add special qualities to areas such as function declarations.

## Motivation

By supporting annotations, we are able to forwards compatibly add new behavior to functions and other statements, and potentially support user specified decorators.

## Design

Let's suppose we want to add the equivalent to [`#[must_use]`](https://doc.rust-lang.org/reference/attributes/diagnostics.html#the-must_use-attribute). We *could* try to create some special syntax for that, like so:

```lua
local function giveValue(): number mustuse
```

This would work in the moment, but let's suppose we also now want to add something like [`#[inline]`](https://doc.rust-lang.org/reference/attributes/codegen.html#the-inline-attribute). We *could* make a special syntax for that as well, like...

```lua
local inline function giveValue(): number mustuse
```

Then suppose we want something like [`noexcept`](https://en.cppreference.com/w/cpp/language/noexcept_spec), we could make a special syntax for that too, but you should be seeing the issue by now: all of these additions are not obvious readability wins, and increase burden of implementation with more parser edge cases.

Instead, this RFC proposes a general syntax of `@name` as an annotation syntax.

Our previous example might instead look like:

```lua
@must_use
@inline
local function giveValue(): number
```

Annotations would support the following syntax:

```
annotation = '@' NAME [ '(' exp ')' ]
```

This annotation would apply to the entire following statement or expression. This would let us support something like:

```lua
@allow("unused_variable")
local x = 1

local x = {
	@allow("undefined_variable")
	notDefined,
}
```

While not specified in this RFC, this could also pave the way for user defined decorators, and the syntax is function-like accordingly.

This RFC does not propose any annotations, just that the syntax be reserved for future use cases.

## Drawbacks

We might not have enough valid use cases to justify adding this to the language itself. Similarly, this might encourage more feature bloat.

On that note, it might be easiest to afford feature specific syntax. For instance, the `mustuse` qualifier, or something like `MustUse<T>`.

## Alternatives

We could limit this only to function declarations.

We could decide to not support user defined decorators, and simplify the syntax accordingly--`@allow("undefined_variable")` could become `@allow(undefined_variable)`, for instance, knowing there will never be any ambiguity in whether or not this is referencing a variable.
