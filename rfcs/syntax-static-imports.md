# Static imports

## Summary

TODO: One paragraph explanation of the feature.

## Motivation

Currently, Luau follows Lua convention by using the `require()` standard library function for importing values from other code files. Later, this syntax was retrofitted to support importing types statically at compile time, so long as the module path can be statically resolved and is constant.

We have begun to see issues with the automatic type importing behaviour. It is not currently possible to import just the types of a module. Instead, the whole module must be required. This means that types often have to be extracted into their own separate modules to avoid cyclic require loops, which means moving the definition of types far away from the places they are being used.

By this point, the Luau developer community have largely settled into a philosophy: `require()` all of your modules at the top of your file, and treat them as you would treat a static import expression. This is so pervasive at this point, that it is worth considering if it is appropriate to formalise this with syntax, and properly distinguish it from the few rare cases where non-statically-resolvable importing is still used.

In addition to the above, relying on detecting `require()` calls in code restricts the possibility to extend importing behaviour to cover newly emerging desires for convenient import syntax and extended features.

As codebases get more complex, we have seen a great increase in unwieldy header code dealing with massive amounts of imports. For example, when dealing with DSL-like libraries that require lots of members to become ergonomic in usage, it's not uncommon to see large numbers of API members being imported in giant blocks at the top of a file:

```Lua
local Fusion = require(Package.Libraries.Fusion)
local Value, Observer, Computed, ForKeys, ForValues, ForPairs, peek = Fusion.Value, Fusion.Observer, Fusion.Computed, Fusion.ForKeys, Fusion.ForValues, Fusion.ForPairs, Fusion.peek
local New, Children, OnEvent, OnChange, Out, Ref, Cleanup = Fusion.New, Fusion.Children, Fusion.OnEvent, Fusion.OnChange, Fusion.Out, Fusion.Ref, Fusion.Cleanup
local Tween, Spring = Fusion.Tween, Fusion.Spring
```

Previously, these issues were solved using `getfenv()`, `_G` or other similar features to try and 'inject' commonly used values from other modules. Since these are not good practice to depend upon - and can even lead to deoptimisation scenarios - there is a void left unfilled for codebases that previously would depend on these features for ergonomic reasons.

Another issue the above code block points to, is the lack of utilites available for destructuring imports. RFCs exist attempting to implement general value destructuring, but they are running into difficulties and ultimately could very well be too difficult to implement cleanly without sacrificing the conciseness that destructuring is pursued for in the first place. It should be noted that a likely-prominent use case for destructuring would be for extracting specific members from imported modules in a convenient way.

The hope is, by exploring systems beyond `require()`, we can open up space to explore these ideas in a way that can preserve backwards compatibility much more easily, without ruining the potential for conciseness by having to work around existing syntax quirks.

## Design

I propose the addition of a static "import statement" to supersede almost all of the current use of `require()` in static contexts. *Almost* all, because there are many valid use cases for wanting to dynamically import modules, and these use cases are outside the scope of this change.

```Lua
!import "foo/bar/baz"
```

The `!import` used here is designed to mirror the current `export` used for exporting type annotations. To avoid ambiguity with user-defined functions, the exclamation point is added, which lines up in style with the current `--!strict` style. This syntax may perhaps be extended in the future to cover other types of static declaration - perhaps even the aforementioned type checking mode.

Since this is a statement rather than an expression (and thus unlike `require()`), it cannot be used inside of calculations. This is because it is intended to introduce members to the namespace statically, not to return a value dynamically:

```Lua
local foo = doSomething(require("foo")) -- ok
local bar = doSomething(!import "foo") -- not ok
```

Since this statement is statically evaluated, the argument *must* be statically evaluatable (as is currently done to provide typechecking for `require()`):

```Lua
!import "foo" -- ok

local bar = "foo"
!import bar -- ok

local bar = tostring(os.clock())
!import bar -- not ok
```

The reason this is not specifically limited to string literals is to allow Roblox-like environments to evaluate statements for their imports:

```Lua
!import script.Parent.Libraries.Fusion

local Package = script.Parent
!import Package.Libraries.Fusion
!import Package:FindFirstChild("Libraries"):FindFirstChild("Fusion")
!import Package:WaitForChild("Libraries"):WaitForChild("Fusion")
```

Multiple forms of import statement can be provided to import as much or as little of a module as required, to address previously mentioned problems with cyclic requires for type-only imports.

### Basic form

The basic form most directly mimics the existing `require()` shape:

```Lua
!import "foo/bar/baz"
```

This places the returned value from the module into a local variable of the same name as the module. Additionally, types from the module are imported into a namespace with the module's name for type analysis, as is done today with `require()`.

That is to say - in a static context - the above snippet can be seen as equivalent to:

```Lua
local baz = require("foo/bar/baz")
```

### Renaming

If a different name should be used for the import destination, perhaps to avoid a namespace conflict, an equals sign can be used:

```Lua
!import not_baz = "foo/bar/baz"
```

The local variable and type namespace are renamed to match the given identifier.

That is to say, these two snippets are equivalent:

```Lua
!import not_baz = "foo/bar/baz"
```

```Lua
local not_baz = require("foo/bar/baz")
```

### Type prefix

If the user wishes to avoid running the imported module at runtime (e.g. to avoid cyclic dependencies), then it should be possible to only import type information which can be statically resolved. To do this, the `type` prefix is used:

```Lua
!import type "foo/bar/baz"
!import not_baz = type "foo/bar/baz"
```

There is currently no equivalent code snippet for this. The closest is ['type smuggling' as presented by Anaminus](https://twitter.com/Anaminus/status/1585287008661938180), which does not work with exported types:

```Lua
!import type "foo/bar/baz"
```

```Lua
type baz = typeof(require("foo/bar/baz"))
```

### Local prefix

Especially in the case of DSL-like libraries (or any libraries that wish to include a prelude of commonly used members) it may be desirable to directly import members into the current namespace, effectively destructuring the module. For this, the `local` keyword may be used, in the same manner as `type` is used.

```Lua
!import local "foo/bar/baz"
!import local type "foo/bar/baz"
```

While there is no syntax ambiguity, the `local` prefix is not sensible with renaming, because it does not make sense to rename a namespace that will not be created. This case should likely warn, but is not necessarily a failure case.

```Lua
!import not_baz = local "foo/bar/baz" -- why?
```

There is no idiomatic equivalent code snippet, though similar behaviour can be achieved with unidiomatic use of `getfenv()`:

```Lua
!import local "foo/bar/baz"
```

```Lua
local _temp = require("foo/bar/baz")
for ident, value in _temp do
    getfenv()[ident] = value
end
_temp = nil
```

### Member list

It may be desirable to further refine what is imported, which can help audit what members the code has access to, or prevent namespace pollution. For this, the `in` keyword may be used, and a list of identifiers given:

```Lua
!import thing1, type thing2, local thing3 in "foo/bar/baz"
!import not_baz = thing1, type thing2, local thing3 in "foo/bar/baz"
```

`type` and `local` prefixes are specified per-identifier. This allows an identically-named value/type pair to be addressed separately, and allows for select members to be elevated into the current namespace rather than being nested inside the module namespace.

If all imported members are `local`, then the module's namespace is not created. For reasons similar to previously, this makes renaming not sensible, and so this should probably warn:

```Lua
!import not_baz = local thing1, local thing2, local thing3 in "foo/bar/baz" -- why?
```

These two snippets are equivalent:

```Lua
!import thing1, thing2, thing3 in "foo/bar/baz"
```

```Lua
local baz = {
    thing1 = require("foo/bar/baz").thing1,
    thing2 = require("foo/bar/baz").thing2,
    thing3 = require("foo/bar/baz").thing3
}
```

## Drawbacks

TODO: Why should we *not* do this?

## Alternatives

I considered using `local import` to allow the declaration of these statements after user declarations. While this looks more stylistically in-line with existing statements and still preserves backwards compatibility, I decided against this because it seemed unwieldy and bulky.

I considered using `--!import` to better align with `--!strict` et al, but decided against it because it did not make it clear that this statement would affect the execution of the code, and generally is not in the spirit of how comments should work.

If we do not do this, then it will remain difficult to extend or make changes to Luau's module importing system, as `require()` has a lot of legacy usage and is used in highly dynamic and user-extendable environments which pose challenging problems for backwards compatibility. This means it may be difficult - or even impossible - to adequately meet demands of large codebases using Luau today and into the future.