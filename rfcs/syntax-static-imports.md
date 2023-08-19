# Static import syntax

## Summary

Improve ergonomics of importing modules and open room for extended importing features by introducing an import statement, to replace a majority of `require()` calls in Luau codebases while respecting backwards compatibility concerns.

## Motivation

Currently, Luau follows Lua convention by using the `require()` standard library function for importing values from other code files. Later, this syntax was retrofitted to support importing types statically at compile time, so long as the module path can be statically resolved and is constant.

Some users have begun to see issues with the automatic type importing behaviour. Specifically, it is not currently possible to import only the types of a module. Instead, the whole module must be required, meaning it is invoked at runtime. 

By this point, the Luau developer community have largely settled into a philosophy: `require()` all of your modules at the top of your file, and treat them as you would treat a static import expression in any other language. This is so pervasive at this point, that it is worth considering if it is appropriate to formalise this with syntax, and properly distinguish it from the few rare cases where non-statically-resolvable importing is still used.

In addition to the above, relying on detecting `require()` calls in code restricts the possibility to extend importing behaviour to cover newly emerging desires for convenient import syntax and extended features.

As codebases get more complex, we have seen a great increase in unwieldy header code dealing with massive amounts of imports. For example, when dealing with DSL-like libraries that require lots of members to become ergonomic in usage, it's not uncommon to see large numbers of API members being imported in giant blocks at the top of a file:

```Lua
local Fusion = require(Package.Libraries.Fusion)
local Value, Observer, Computed, ForKeys, ForValues, ForPairs, peek = Fusion.Value, Fusion.Observer, Fusion.Computed, Fusion.ForKeys, Fusion.ForValues, Fusion.ForPairs, Fusion.peek
local New, Children, OnEvent, OnChange, Out, Ref, Cleanup = Fusion.New, Fusion.Children, Fusion.OnEvent, Fusion.OnChange, Fusion.Out, Fusion.Ref, Fusion.Cleanup
local Tween, Spring = Fusion.Tween, Fusion.Spring
```

Previously, these issues were solved using `getfenv()`, `_G` or other similar features to try and 'inject' commonly used values from other modules. Since these are not good practice to depend upon - and can even lead to deoptimisation scenarios - there is a void left unfilled for codebases that previously would depend on these features for ergonomic reasons.

The primary issue the above code block points to, is the lack of utilites available for destructuring imports. [RFCs exist attempting to implement general value destructuring](https://github.com/Roblox/luau/pull/629), but they are running into difficulties and ultimately could very well be too difficult to implement cleanly without sacrificing the conciseness that destructuring is pursued for in the first place. It should be noted that a likely-prominent use case for destructuring would be for extracting specific members from imported modules in a convenient way.

The hope is, by exploring systems beyond `require()`, we can open up space to explore these ideas in a way that can preserve backwards compatibility much more easily, without ruining the potential for conciseness by having to work around existing syntax quirks.

## Design

This RFC proposes the addition of a static "import statement" to supersede almost all of the current use of `require()` in static contexts. *Almost* all, because there are many valid use cases for wanting to dynamically import modules, and these use cases are outside the scope of this change.

```Lua
import from "foo/bar/baz"
```

The `import` used here is designed to mirror the current `export` used for exporting type annotations. To avoid ambiguity with function call syntax, the `from` keyword is added, which distinguishes the statement from function call syntax with a string literal.

Unlike `require()` it is intended to introduce members to the namespace, not to return a value dynamically. This is why it is specifically a statement, not an expression:

```Lua
local foo = doSomething(require("foo")) -- ok
local bar = doSomething(import from "foo") -- not ok
```

The module path is evaluated at runtime, but is required to be analysable for the purposes of static type checking. Specifically, imports are *not* hardcoded at compile time, because especially in Roblox-like environments, the same code may execute from different locations.

```Lua
import from "foo" -- ok

local bar = "foo"
import from bar -- ok

local bar = tostring(os.clock())
import from bar -- not ok
```

The reason this is not specifically limited to string literals is to allow Roblox-like environments to evaluate statements for their imports, including the use of previously defined constant-like values:

```Lua
import from script.Parent.Libraries.Fusion

local Package = script.Parent
import from Package.Libraries.Fusion
import from Package:FindFirstChild("Libraries"):FindFirstChild("Fusion")
import from Package:WaitForChild("Libraries"):WaitForChild("Fusion")
```

### Syntax: Basic form

The most basic form gives only the expression which resolves statically to the module path:

```Lua
import from "foo/bar/baz"
```

The returned value from the module is placed in a local variable, adopting the module's name. Types are placed in a namespace of the same name. This is how `require()` works today.

The above snippet can be seen as equivalent to:

```Lua
local baz = require("foo/bar/baz")
```

### Syntax: Renaming

To use a different name for the local variable/type namespace, an equals sign is added at the end of the statement:

```Lua
import from "foo/bar/baz" = not_baz
```

Beyond user convenience, this allows code to deal with modules named identically, as can be done today with `require()`.

The above snippet can be seen as equivalent to:

```Lua
local not_baz = require("foo/bar/baz")
```

### Syntax: Type prefix

To import only the types from a module, and skip evaluating the module at runtime, the `type` keyword can be added.

```Lua
import type from "foo/bar/baz"
import type from "foo/bar/baz" = not_baz
```

There is currently no exactly equivalent code snippet for this. The closest is ['type smuggling' as presented by Anaminus](https://twitter.com/Anaminus/status/1585287008661938180), which does not work with exported types:

```Lua
type baz = typeof(require("foo/bar/baz"))
```

### Syntax: Local prefix

To destructure an import and insert its contents directly into the current namespace, the `local` keyword can be added (before `type` if present):

```Lua
import local from "foo/bar/baz"
import local type from "foo/bar/baz"
```

This is especially useful in the case of DSL-like libraries, or any libraries that wish to include a prelude of commonly used members. It is acknowledged that this can lead to namespace pollution, but this is something the developer is in control of at all times, and explicitly opts into.

Unless only types are being imported, the module must return a table. All statically resolvable members of the table, which have string keys and are valid identifiers, are turned into local variables in the current namespace.

While there is no syntax ambiguity, the `local` prefix is not sensible with renaming, because it does not make sense to rename a namespace that will not be created. This case should likely warn, but is not necessarily a failure case.

```Lua
import local from "foo/bar/baz" = not_baz -- why?
```

There is no equivalent code snippet, though similar behaviour without type importing can be achieved with unidiomatic use of `getfenv()`:

```Lua
local _temp = require("foo/bar/baz")
for ident, value in _temp do
    getfenv()[ident] = value
end
_temp = nil
```

### Syntax: Member list

To only import certain members from a module, their identifiers can be listed:

```Lua
import thing1, type thing2, local thing3 from "foo/bar/baz"
import not_baz = thing1, type thing2, local thing3 from "foo/bar/baz"
```

Unless only types are being imported, the module must return a table. All of the non-type identifiers in the list should correspond with statically resolvable members inside of that table.

`type` and `local` prefixes are specified per-identifier. This allows an identically-named value/type pair to be addressed separately. This also allows developers to keep namespace pollution under control if there are only select members they wish to import into the current namespace.

If all imported members are `local`, then the module's namespace is not created. For reasons similar to previously, this makes renaming not sensible, and so this should probably warn:

```Lua
import local thing1, local thing2, local thing3 from "foo/bar/baz" = not_baz -- why?
```

These two snippets are equivalent:

```Lua
import thing1, thing2, thing3 from "foo/bar/baz"
```

```Lua
local baz = {
    thing1 = require("foo/bar/baz").thing1,
    thing2 = require("foo/bar/baz").thing2,
    thing3 = require("foo/bar/baz").thing3
}
```

### Example usage

Simple usage becomes shorter and deduplicates the module name, automatically enforcing the convention that modules are imported to a variable of the same name, and allowing adjacent module imports to align visually and reveal common paths without using whitespace:

```Lua
local Package = script.Parent

local FittedLength = require(Package.Libraries.Layman.Layout.GroupOp.Smart.FittedLength)
local Stack = require(Package.Libraries.Layman.Layout.GroupOp.Stack)
local Renderable2D = require(Package.Libraries.Layman.Element.Traits.Renderable2D)
```

```Lua
local Package = script.Parent

import from Package.Libraries.Layman.Layout.GroupOp.Smart.FittedLength
import from Package.Libraries.Layman.Layout.GroupOp.Stack
import from Package.Libraries.Layman.Element.Traits.Renderable2D
```

Granular imports are made simpler, and remove boilerplate type declarations which are forced to depend on the generics declared on the original type:

```Lua
local SuiteUI = script.Parent
local Layman = SuiteUI.Parent.Layman

local Fusion = require(SuiteUI.Parent.Fusion)
local Computed = Fusion.Computed
local New = Fusion.New
type CanBeState<T> = Fusion.CanBeState<T>

local Element = require(Layman.Element)
local WithExtents = require(Layman.Extents.WithExtents)
local Renderable2D = require(Layman.Element.Traits.Renderable2D)
type RenderProps2D = Renderable2D.RenderProps2D
```

```Lua
local SuiteUI = script.Parent
local Layman = SuiteUI.Parent.Layman

import local Computed, local New, local type CanBeState from SuiteUI.Parent.Fusion
import from Layman.Element
import from Layman.Extents.WithExtents
import local type RenderProps2D from Layman.Element.Traits.Renderable2D
```

DSL-like libraries, whose preludes previously had to be manually destructured, can enjoy new conciseness, and no longer have to continually synchronise their header code as new members are added to the library. It is anticipated that a future convention may be to include 'prelude' sub-modules in libraries which re-export the most common constructs, values and types, as is done in other ecosystems such as Rust, to allow them to be imported in one step:

```Lua
local Fusion = require(Package.Libraries.Fusion)
local Value, Observer, Computed, ForKeys, ForValues, ForPairs, peek = Fusion.Value, Fusion.Observer, Fusion.Computed, Fusion.ForKeys, Fusion.ForValues, Fusion.ForPairs, Fusion.peek
local New, Children, OnEvent, OnChange, Out, Ref, Cleanup = Fusion.New, Fusion.Children, Fusion.OnEvent, Fusion.OnChange, Fusion.Out, Fusion.Ref, Fusion.Cleanup
local Tween, Spring = Fusion.Tween, Fusion.Spring
```

```Lua
import local from Package.Libraries.Fusion
```

Individual functions, `do` blocks or other scopes can statically import into their namespace without spilling imports to other code and without introducing extra scopes or indentation. The imports are resolved ahead of time, so they need not even call into C code more than once:

```Lua
do
    local fast_eq = require(Package.Libraries.fast_eq)
    function AbstractLayer:fast_eq(other)
        return fast_eq(self._ir, other._ir)
    end
end
-- fast_eq is not accessible here
```

```Lua
function AbstractLayer:fast_eq(other)
    import local from Package.Libraries.fast_eq
    return fast_eq(self._ir, other._ir)
end
-- fast_eq is not accessible here
```

## Drawbacks

`require()` keeps in line with existing Lua 5.1 codebases, and already serves the basic function of importing modules. It may be nice to keep the consistency between static and dynamic imports, even at the expense of some of the features listed here. It's easier to understand where values are imported to when expressed in a familiar `local x = y` construct, though admittedly this does not quite extend to type importing.

Statically evaluated statements might feel 'out of step' with Lua's dynamic nature. Even though an attempt has been made at ensuring it does not become confused with dynamic statements, the idea of statically evaluated statements might still not necessarily fit the philosophy of the language at all. It may be argued that it is instead better to try and guess user intent from predictable patterns in the usage of dynamic code patterns, rather than trying to make areas used in static analysis explicit.

The extensions to the `!import` syntax, such as renaming or destructuring, may be seen as a measurable increase in complexity from what was previously a simple and predictable operation. Depending on the syntax and keywords used, these extended features may run the risk of confusing newer users, or making the way code is imported less immediately clear.

While efforts have been made to align this feature to the kinds of analysis already done internally by Luau's tooling, it undeniably still introduces internal complexity. Even though these statements are more explicitly designed for static analysis and useful type inference compared to the more dynamic and unpredictable `require()`, backwards compatibility concerns mean that the more complex logic for detecting `require()` usage still needs to be maintained, and cannot be removed even if it were to be superseded by a more predictable form. In addition, some of the extended importing features are novel, and do not correspond to existing language features, which introduces new internal considerations that were not present before.

While the developer retains complete control over which members are imported by selectively `local`-ing desired members to the current namespace (or avoiding the feature entirely), it is appreciable that the `local` syntax without a list of identifiers would introduce some level of implicitness and. This is both a feature and a bug - this is explicitly what is wanted (and very highly so) for users of DSL-like libraries, while it is also a potential semver hazard. Scoping and order of variable initialisation become important in this case; imports overwrite variable declarations before them (which may break future users), but variable declarations equally overwrite imports before them (which does not). Since imports are generally kept at the top of the code file, I do not think these are worrisome enough breaking changes, except in cases where users are using globals or function environments, which are uncommon and unidiomatic anyway.

Some external tooling operates per-file, which does not necessarily align with the `local` syntax because the names of the imported members are implied. However, these tools are also already incapable of modelling Luau's current implicit inferences between files, which this system was modelled to mirror, so the importance of this point is not certain and should be discussed.

## Alternatives

A comment/preprocesser style `--!import` would better align with `--!strict` et al, but this was decided against because it did not make clear that this statement would affect the execution of the code, and generally is not in the spirit of how comments should work.

It may instead be more appropriate to try and investigate whether the extended features of this import statement can be better addressed by more general features such as generalised destructuring of values at runtime. However, these RFCs appear to struggle to reconcile syntax desires with backwards compatibility restrictions. [The RFC can be found here.](https://github.com/Roblox/luau/pull/629).

If we do not do this, then it will remain difficult to extend or make changes to Luau's module importing system, as `require()` has a lot of legacy usage and is used in highly dynamic and user-extendable environments which pose challenging problems for backwards compatibility. This means it may be difficult - or even impossible - to adequately meet demands of large codebases using Luau today and into the future.
