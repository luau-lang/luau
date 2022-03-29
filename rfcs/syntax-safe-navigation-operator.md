# Safe navigation postfix operator (?)

**Note**: We have unresolved issues with interaction between this feature and Roblox instance hierarchy. This may affect the viability of this proposal.

## Summary

Introduce syntax to navigate through `nil` values, or short-circuit with `nil` if it was encountered.


## Motivation

nil values are very common in Lua, and take care to prevent runtime errors. 

Currently, attempting to index `dog.name` while caring for `dog` being nil requires some form of the following:

```lua
local dogName = nil
if dog ~= nil then
    dogName = dog.name
end
```

...or the unusual to read...

```lua
local dogName = dog and dog.name
```

...which will return `false` if `dog` is `false`, instead of throwing an error because of the index of `false.name`.

Luau provides the if...else expression making this turn into:

```lua
local dogName = if dog == nil then nil else dog.name
```

...but this is fairly clunky for such a common expression.

## Design

The safe navigation operator will make all of these smooth, by supporting `x?.y` to safely index nil values. `dog?.name` would resolve to `nil` if `dog` was nil, or the name otherwise.

The previous example turns into `local dogName = dog?.name` (or just using `dog?.name` elsewhere).

Failing the nil-safety check early would make the entire expression nil, for instance `dog?.body.legs` would resolve to `nil` if `dog` is nil, rather than resolve `dog?.body` into nil, then turning into `nil.legs`.

```lua
dog?.name --[[ is the same as ]] if dog == nil then nil else dog.name
```

The short-circuiting is limited within the expression.

```lua
dog?.owner.name -- This will return nil if `dog` is nil
(dog?.owner).name -- `(dog?.owner)` resolves to nil, of which `name` is then indexed. This will error at runtime if `dog` is nil.

dog?.legs + 3 -- `dog?.legs` is resolved on its own, meaning this will error at runtime if it is nil (`nil + 3`)
```

The operator must be used in the context of either a call or an index, and so:

```lua
local value = x?
```

...would be invalid syntax. 

This syntax would be based on expressions, and not identifiers, meaning that `(x or y)?.call()` would be valid syntax.

### Type
If the expression is typed as an optional, then the resulting type would be the final expression, also optional. Otherwise, it'll just be the resulting type if `?` wasn't used.

```lua
local optionalObject: { name: string }?
local optionalObjectName = optionalObject?.name -- resolves to `string?`

local nonOptionalObject: { name: string }
local nonOptionalObjectName = nonOptionalObject?.name -- resolves to `string`
```

### Calling

This RFC only specifies `x?.y` as an index method. `x?:y()` is currently unspecified, and `x?.y(args)` as a syntax will be reserved (will error if you try to use it).

While being able to support `dog?.getName()` is useful, it provides [some logistical issues for the language](https://github.com/Roblox/luau/pull/142#issuecomment-990563536).

`x?.y(args)` will be reserved both so that this can potentially be resolved later down the line if something comes up, but also because it would be a guaranteed runtime error under this RFC: `dog?.getName()` will first index `dog?.getName`, which will return nil, then will attempt to call it.

### Assignment
`x?.y = z` is not supported, and will be reported as a syntax error.

## Drawbacks

As with all syntax additions, this adds complexity to the parsing of expressions, and the execution of cancelling the rest of the expression could prove challenging.

Furthermore, with the proposed syntax, it might lock off other uses of `?` within code (and not types) for the future as being ambiguous.

## Alternatives

Doing nothing is an option, as current standard if-checks already work, as well as the `and` trick in other use cases, but as shown before this can create some hard to read code, and nil values are common enough that the safe navigation operator is welcome.

Supporting optional calls/indexes, such as `x?[1]` and `x?()`, while not out of scope, are likely too fringe to support, while adding on a significant amount of parsing difficulty, especially in the case of shorthand function calls, such as `x?{}` and `x?""`.

It is possible to make `x?.y = z` resolve to only setting `x.y` if `x` is nil, but assignments silently failing can be seen as surprising.
