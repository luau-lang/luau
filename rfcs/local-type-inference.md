# Local Type Inference

## Summary

We are going to supplant the current type solver with one based on Benjamin Pierce's Local Type Inference algorithm:

https://www.cis.upenn.edu/~bcpierce/papers/lti-toplas.pdf

## Motivation

Luau's type inference algorithm is used for much more than typechecking scripts.  It is also the backbone of an autocomplete algorithm which has to work even for people who don't know what types or type systems are.

We originally implemented nonstrict mode by making some tactical adjustments to the type inference algorithm.  This was great for reducing false positives in untyped code, but carried with it the drawback that the inference result was usually not good enough for the autocomplete system.  In order to offer a high quality experience, we've found ourselves to run type inference on nonstrict scripts twice: once for error feedback, and once again to populate the autocomplete database.

Separately, we would also like more accurate type inference in general.  Our current type solver jumps to conclusions a little bit too quickly.  For example, it cannot infer an accurate type for an ordinary search function:

```lua
function index_of(tbl, el)
    for i = 0, #tbl do
        if tbl[i] == el then
            return i
        end
    end
    return nil
end
```

Our solver sees two `return` statements and assumes that, because the first statement yields a `number`, so too must the second.

To fix this, we are going to move to an architecture where type inference and type checking are two separate steps.  Whatever mode the user is programming with, we will run an accurate type inference pass over their code and then run one of two typechecking passes over it.

## Notation

We'll use the standard notation `A <: B` to indicate that `A` is a subtype of `B`.

## Design

At a very high level, local type inference is built around the idea that we track the lower bounds and their upper bounds.  The lower bounds of a binding is the set of values that it might conceivably receive.  If a binding receives a value outside of its upper bounds, the program will fail.

At the implementation level, we reencode free types as the space between these bounds.

Upper bounds arise only from type annotations and certain builtin operations whereas lower bounds arise from assignments, return statements, and uses.

Free types all start out with bounds `never <: 't <: unknown`.  Intuitively, we say that `'t` represents some set of values whose domain is at least `never` and at most `unknown`.  This naturally could be any value at all.

When dispatching a constraint `T <: 't`, we replace the lower bounds of `'t` by the union of its old lower bounds and `T`.  When dispatching a constraint `'t <: T`, we replace the upper bounds by its upper bound intersected with `T`.  In other words, lower bounds grow from nothing as we see the value used whereas the upper bound initially encompasses everything and shrinks as we constrain it.

### Constraint Generation Rules

A return statement expands the lower bounds of the enclosing function's return type.

```lua
function f(): R
    local x: X
    return x
    -- X <: R
end
```

An assignment adds to the lower bounds of the assignee.

```lua
local a: A
local b: B
a = b
-- B <: A
```

A function call adds to the upper bounds of the function being called.

Equivalently, passing a value to a function adds to the upper bounds of that value and to the lower bounds of its return value.

```lua
local g
local h: H
local j = g(h)
-- G <: (H) -> I...
-- I... <: J
```

Property access is a constraint on a value's upper bounds.
```lua
local a: A
a.b = 2
-- A <: {b: number}

a[1] = 3
-- A <: {number}
```

### Generalization

Generalization is the process by which we infer that a function argument is generic.  Broadly speaking, we solve constraints that arise from the function interior, we scan the signature of the function for types that are unconstrained, and we replace those types with generics.  This much is all unchanged from the old solver.

Unlike with the old solver, we never bind free types when dispatching a subtype constraint under local type inference.  We only bind free types during generalization.

If a type only appears in covariant positions in the function's signature, we can replace it by its lower bound.  If it only appears in contravariant positions, we replace it by its upper bound.  If it appears in both, we'll need to implement bounded generics to get it right.  This is beyond the scope of this RFC.

If a free type has neither upper nor lower bounds, we replace it with a generic.

Some simple examples:

```lua
function print_number(n: number) print(n) end

function f(n)
    print_number(n)
end
```

We arrive at the solution `never <: 'n <: number`.  When we generalize, we can replace `'n` by its upper bound, namely `number`.  We infer `f : (number) -> ()`.

Next example:

```lua
function index_of(tbl, el)      -- index_of : ('a, 'b) -> 'r
    for i = 0, #tbl do          -- i : number
        if tbl[i] == el then    -- 'a <: {'c}
            return i            -- number <: 'r
        end
    end
    return nil                  -- nil <: 'r
end
```

When typechecking this function, we have two constraints on `'r`, the return type.  We can combine these constraints by taking the union of the lower bounds, leading us to `number | nil <: 'r <: unknown`.  The type `'r` only appears in the return type of the function.  The return type of this function is `number | nil`.

At runtime, Luau allows any two values to be compared.  Comparisons of values of mismatched types always return `false`.  We therefore cannot produce any interesting constraints about `'b` or `'c`.

We end up with these bounds:

```
never        <: 'a <: {'c}
never        <: 'b <: unknown
never        <: 'c <: unknown
number | nil <: 'r <: unknown
```

`'a` appears in the argument position, so we replace it with its upper bound `{'c}`.  `'b` and `'c` have no constraints at all so they are replaced by generics `B` and `C`.  `'r` appears only in the return position and so is replaced by its lower bound `number | nil`.

The final inferred type of `index_of` is `<B, C>({C}, B) -> number | nil`.

## Drawbacks

This algorithm requires that we create a lot of union and intersection types.  We need to be able to consistently pare down degenerate unions like `number | number`.

Local type inference is also more permissive than what we have been doing up until now.  For instance, the following is perfectly fine:

```lua
local x = nil
if something then
    x = 41
else
    x = "fourty one"
end
```

We'll infer `x : number | string | nil`.  If the user wishes to constrain a value more tightly, they will have to write an annotation.

## Alternatives

### What TypeScript does

TypeScript very clearly makes it work in what we would call a strict mode context, but we need more in order to offer a high quality nonstrict mode.  For instance, TypeScript's autocomplete is completely helpless in the face of this code fragment:

```ts
let x = null;
x = {a: "a", b: "pickles"};
x.
```

TypeScript will complain that the assignment to `x` is illegal because `x` has type `null`.  It will further offer no autocomplete suggestions at all when the user types the final `.`.

It's not viable for us to require users to write type annotations.  Many of our users do not yet know what types are but we are nevertheless committed to providing them a tool that is helpful to them.

### Success Typing

Success typing is the algorithm used by the Dialyzer inference engine for Erlang.  Instead of attempting to prove that values always flow in sensible ways, it tries to prove that values _could_ flow in sensible ways.

Success typing is quite nice in that it's very forgiving and can draw surprisingly useful information from untyped code, but that forgiving nature works against us in the case of strict mode.
