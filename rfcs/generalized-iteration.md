# Generalized iteration

**Status**: Implemented

## Summary

Introduce support for iterating over tables without using `pairs`/`ipairs` as well as a generic customization point for iteration via `__iter` metamethod.

## Motivation

Today there are many different ways to iterate through various containers that are syntactically incompatible.

To iterate over arrays, you need to use `ipairs`: `for i, v in ipairs(t) do`. The traversal goes over a sequence `1..k` of numeric keys until `t[k] == nil`, preserving order.

To iterate over dictionaries, you need to use `pairs`: `for k, v in pairs(t) do`. The traversal goes over all keys, numeric and otherwise, but doesn't guarantee an order; when iterating over arrays this may happen to work but is not guaranteed to work, as it depends on how keys are distributed between array and hash portion.

To iterate over custom objects, whether they are represented as tables (user-specified) or userdata (host-specified), you need to expose special iteration methods, for example `for k, v in obj:Iterator() do`.

All of these rely on the standard Lua iteration protocol, but it's impossible to trigger them in a generic fashion. Additionally, you *must* use one of `pairs`/`ipairs`/`next` to iterate over tables, which is easy to forget - a naive `for k, v in tab do` doesn't work and produces a hard-to-understand error `attempt to call a table value`.

This proposal solves all of these by providing a way to implement uniform iteration with self-iterating objects by allowing to iterate over objects and tables directly via convenient `for k, v in obj do` syntax, and specifies the default iteration behavior for tables, thus mostly rendering `pairs`/`ipairs` obsolete - making Luau easier to use and teach.

## Design

In Lua, `for vars in iter do` has the following semantics (otherwise known as the iteration protocol): `iter` is expanded into three variables, `gen`, `state` and `index` (using `nil` if `iter` evaluates to fewer than 3 results); after this the loop is converted to the following pseudocode:

```lua
while true do
  vars... = gen(state, index)
  index = vars... -- copy the first variable into the index
  if index == nil then break end
  
  -- loop body goes here
end
```

This is a general mechanism that can support iteration through many containers, especially if `gen` is allowed to mutate state. Importantly, the *first* returned variable (which is exposed to the user) is used to continue the process on the next iteration - this can be limiting because it may require `gen` or `state` to carry extra internal iteration data for efficiency. To work around this for table iteration to avoid repeated calls to `next`, Luau compiler produces a special instruction sequence that recognizes `pairs`/`ipairs` iterators and stores the iteration index separately.

Thus, today the loop `for k, v in tab do` effectively executes `k, v = tab()` on the first iteration, which is why it yields `attempt to call a table value`. If the object defines `__call` metamethod then it can act as a self-iterating method, but this is not idiomatic, not efficient and not pure/clean.

This proposal comes in two parts: general support for `__iter` metamethod and default implementation for tables without one. With both of these in place, there's going to be a single, idiomatic, general and performant way to iterate through the object of any type:

```lua
for k, v in obj do
...
end
```

### __iter

To support self-iterating objects, we modify the iteration protocol as follows: instead of simply expanding the result of expression `iter` into three variables (`gen`, `state` and `index`), we check if the first result has an `__iter` metamethod (which can be the case if it's a table, userdata or another composite object (e.g. a record in the future). If it does, the metamethod is called with `gen` as the first argument, and the returned three values replace `gen`/`state`/`index`. This happens *before* the loop:

```lua
if getmetatable(gen) and getmetatable(gen).__iter then
   gen, state, index = getmetatable(gen).__iter(gen)
end
```

This check is comparatively trivial: usually `gen` is a function, and functions don't have metatables; as such we can simply check the type of `gen` and if it's a table/userdata, we can check if it has a metamethod `__iter`. Due to tag-method cache, this check is also very cheap if the metamethod is absent.

This allows objects to provide a custom function that guides the iteration. Since the function is called once, it is easy to reuse other functions in the implementation, for example here's a node object that exposes iteration through its children:

```lua
local Node = {}
Node.__index = Node

function Node.new(children)
  return setmetatable({ children = children }, Node)
end

function Node:__iter()
  return next, self.children
end
```

Luau compiler already emits a bytecode instruction, FORGPREP*, to perform initial loop setup - this is where we can evaluate `__iter` as well.

Naturally, this means that if the table has `__iter` metamethod and you need to iterate through the table fields instead of using the provided metamethod, you can't rely on the general iteration scheme and need to use `pairs`. This is similar to other parts of the language, like `t[k]` vs `rawget(t, 'k')`, where the default behavior is overrideable but a library function can help peek behind the curtain.

### Default table iteration

If the argument is a table and it does not implement `__iter` metamethod, we treat this as an attempt to iterate through the table using the builtin iteration order.

> Note: we also check if the table implements `__call`; if it does, we fall back to the default handling. We may be able to remove this check in the future, but we will need this initially to preserve backwards compatibility with custom table-driven iterator objects that implement `__call`. In either case, we will be able to collect detailed analytics about the use of `__call` in iteration, and if neither is present we can emit a specialized error message such as `object X is not iteratable`.

To have a single, unified, iteration scheme over tables regardless of whether they are arrays or dictionaries, we establish the following semantics:

- First, the traversal goes over numeric keys in range `1..k` up until reaching the first `k` such that `t[k] == nil`
- Then, the traversal goes over the remaining keys (with non-nil values), numeric and otherwise, in unspecified order.

For arrays with gaps, this iterates until the first gap in order, and the remaining order is not specified.

> Note: This behavior is similar to what `pairs` happens to provide today, but `pairs` doesn't give any guarantees, and it doesn't always provide this behavior in practice.

To ensure that this traversal is performant, the actual implementation of the traversal involves going over the array part (in index order) and then over the hash part (in hash order). For that implementation to satisfy the criteria above, we need to make two additional changes to table insertion/rehash:

- When inserting key `k` in the table when `k == t->sizearray + 1`, we force the table to rehash (resize its array portion). Today this is only performed if the hash portion is full, as such sometimes numeric keys can end up in the hash part.
- When rehashing the table, we ensure that the hash part doesn't contain the key `newsizearray + 1`. This requires checking if the table has this key, which may require an additional hash lookup but we only need to do this in rare cases based on the analysis of power-of-two key buckets that we already collect during rehash.

These changes guarantee that the order observed via standard traversal with `next`/`pairs` matches the guarantee above, which is nice because it means we can minimize the complexity cost of this change by reusing the traversal code, including VM optimizations. They also mean that the array boundary (aka `#t`) can *always* be computed from just the array portion, which simplifies the table length computation and may slightly speed it up.

## Drawbacks

This makes `for` desugaring and implementation a little more complicated; it's not a large complexity factor in Luau because we already have special handling for `for` loops in the VM, but it's something to keep in mind.

While the proposed iteration scheme should be a superset to both `pairs` and `ipairs` for tables, for arrays `ipairs` may in some cases be faster because it stops at the first `nil`, whereas the proposed new scheme (like `pairs`) needs to iterate through the rest of the table's array storage. This may be fixable in the future, if we replace our cached table length (`aboundary`) with Lua 5.4's `alimit`, which maintains the invariant that all values after `alimit` in the array are `nil`. This would make default table iteration maximally performant as well as help us accelerate GC in some cases, but will require extra checks during table assignments which is a cost we may not be willing to pay. Thus it is theoretically possible that we will end up with `ipairs` being a slightly faster equivalent for array iteration forever.

The resulting iteration behavior, while powerful, increases the divergence between Luau and Lua, making more programs that are written for Luau not runnable in Lua. Luau language in general does not consider this type of compatibility essential, but this is noted for posterity.

The changes in insertion behavior that facilitate single iteration order may have a small cost; that said, they are currently understood to belong to paths that are already slow and the added cost is minimal.

The extra semantics will make inferring the types of the variables in a for loop more difficult - if we know the type of the expression that is being iterated through it probably is not a problem though.

## Alternatives

Other major designs have been considered.

A minor variation of the proposal involves having `__iter` be called on every iteration instead of at loop startup, effectively having `__iter` work as an alternative to `__call`. The issue with this variant is that while it's a little simpler to specify and implement, it restricts the options when implementing custom iteratable objects, because it would be difficult for iteratable objects to store custom iteration state elsewhere since `__iter` method would effectively need to be pure, as it can't modify the object itself as more than one concurrent iteration needs to be supported.

A major variation of the proposal involves instead supporting `__pairs` from Lua 5.2. The issue with this variant is that it still requires the use of a library method, `pairs`, to work, which doesn't make the language simpler as far as table iteration, which is the 95% case, is concerned. Additionally, with some rare exceptions metamethods today extend the *language* behavior, not the *library* behavior, and extending extra library functions with metamethods does not seem true to the core of the language. Finally, this only works if the user uses `pairs` to iterate and doesn't work with `ipairs`/`next`.

Another variation involves using a new pseudo-keyword, `foreach`, instead of overloading existing `for`, and only using the new `__iter` semantics there. This can more cleanly separate behavior, requiring the object to have an `__iter` metamethod (or be a table) in `foreach` - which also avoids having to deal with `__call` - but it also requires teaching the users a new keyword which fragments the iteration space a little bit more. Compared to that, the main proposal doesn't introduce new divergent syntax, and merely tweaks existing behavior to be more general, thus making an existing construct easier to use.

Finally, the author also considered and rejected extending the iteration protocol as part of this change. One problem with the current protocol is that the iterator requires an allocation (per loop execution) to keep extra state that isn't exposed to the user. The builtin iterators like `pairs`/`ipairs` work around this by feeding the user-visible index back to the search function, but that's not always practical. That said, having a different iteration protocol in effect only when `__iter` is used makes the language more complicated for unclear efficiency gains, thus this design doesn't suggest a new core protocol to favor simplicity.
