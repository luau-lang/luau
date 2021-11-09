# Safe navigation postfix operator (?)

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

This trick gets worse in the case of calling methods. For example, let's suppose we wanted to call `dog.fetch()`, while `dog` is still potentially nil.

The one-line `and` trick will no longer work (or will get less readable as you try to shoe-horn this in), so we must:

```lua
if dog ~= nil then
    dog.fetch()
end
```

But this gets even worse when it comes to chained indexing. Let's suppose we wanted to run `dog.owner.handshake()`, while `dog` can be nil and `owner` can be nil.

```lua
if dog ~= nil and dog.owner ~= nil then
    dog.owner.handshake()
end
```

...which gets even worse in the context of calling this function in, say, another function:

```lua
-- Oops! dog and dog.owner can be nil
logDogName(getLogger(), dog.name, dog.owner:getDisplayName())

-- In order to preserve this order (assuming argument execution order mattered)...
local logger = getLogger()
local name = dog and dog.name
local displayName

if dog.owner ~= nil then
    displayName = dog.owner:getDisplayName()
end

logDogName(logger, name, displayName)
```

## Design

The safe navigation operator will make all of these smooth, by supporting `x?.y` and similar indexing operators. `dog?.name` would resolve to `nil` if `dog` was nil, or the name otherwise. `owner?.handshake()` would only call `handshake` if `owner` is not nil.

The long example would turn into:

```lua
logDogName(getLogger(), dog?.name, dog?.owner?:getDisplayName())
```

Failing the nil-safety check early would make the entire expression nil, for instance `dog?.body.legs` would resolve to `nil` if `dog` is nil, rather than resolve `dog?.body` into nil, then turning into `nil.legs`.

The list of valid operators to follow the safe navigation operator would be:

```lua
dog?.name --[[ is the same as ]] if dog == nil then nil else dog.name
dog?.getName() --[[ is the same as ]] if dog == nil then nil else dog.getName()
dog?:getName(args) --[[ is the same as ]] if dog == nil then nil else dog:getName(args)
```

When using safe navigation to call a function, the short circuiting will prevent the arguments from being evaluated in the case of nil.

```lua
-- Will NOT call getValue() if `object` is nil
object?.doSomething(getValue())
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

### Assignment
`x?.y = z` is not supported, and will be reported as a syntax error.

## Drawbacks

As with all syntax additions, this adds complexity to the parsing of expressions, and the execution of cancelling the rest of the expression could prove challenging.

Furthermore, with the proposed syntax, it might lock off other uses of `?` within code (and not types) for the future as being ambiguous.

## Alternatives

Doing nothing is an option, as current standard if-checks already work, as well as the `and` trick in other use cases, but as shown before this can create some hard to read code, and nil values are common enough that the safe navigation operator is welcome.

Supporting optional calls/indexes, such as `x?[1]` and `x?()`, while not out of scope, are likely too fringe to support, while adding on a significant amount of parsing difficulty, especially in the case of shorthand function calls, such as `x?{}` and `x?""`.

It is possible to make `x?.y = z` resolve to only setting `x.y` is `x` is nil, but assignments silently failing can be seen as surprising.
