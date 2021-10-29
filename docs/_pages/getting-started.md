---
permalink: /getting-started
title: Getting Started
toc: true
---

To get started with Luau you need to use `luau` command line binary to run your code and `luau-analyze` to run static analysis (including type checking and linting). You can download these from [Artifacts on a recent build](https://github.com/Roblox/luau/actions/workflows/release.yml).

## Creating a script

To create your own testing script, create a new file with `.lua` as the extension:

```lua
function ispositive(x)
    return x > 0
end

print(ispositive(1))
print(ispositive("2"))

function isfoo(a)
    return a == "foo"
end

print(isfoo("bar"))
print(isfoo(1))
```

You can now run the file using `luau test.lua` and analyze it using `luau-analyze test.lua`.

Note that there are no warnings about calling ``ispositive()`` with a string, or calling ``isfoo()`` a number. This is because the type checking uses non-strict mode by default, which is lenient in how it infers types used by the program.

## Type inference

Now modify the script to include ``--!strict`` at the top:

```lua
--!strict

function ispositive(x)
    return x > 0
end

print(ispositive(1))
print(ispositive("2"))
```

In ``strict`` mode, Luau will infer types based on analysis of the code flow. There is also ``nonstrict`` mode, where analysis is more conservative and types are more frequently inferred as ``any`` to reduce cases where legitimate code is flagged with warnings.

In this case, Luau will use the ``return x > 0`` statement to infer that ``ispositive()`` is a function taking a number and returning a boolean. Note that in this case, it was not necessary to add any explicit type annotations.

Based on Luau's type inference, the analysis tool will now flag the incorrect call to ``ispositive()``:

```
$ luau-analyze test.lua
test.lua(7,18): TypeError: Type 'string' could not be converted into 'number'
```

## Annotations

You can add annotations to locals, arguments, and function return types. Among other things, annotations can help enforce that you don't accidentally do something stupid. Here's how we would add annotations to ``ispositive()``:

```lua
--!strict

function ispositive(x : number) : boolean
    return x > 0
end

local result : boolean
result = ispositive(1)

```

Now we've told explicitly told Luau that ``ispositive()`` accepts a number and returns a boolean. This wasn't strictly (pun intended) necessary in this case, because Luau's inference was able to deduce this already. But even in this case, there are advantages to explicit annotations. Imagine that later we decide to change ``ispositive()`` to return a string value:

```lua
--!strict

function ispositive(x : number) : boolean
    if x > 0 then
        return "yes"
    else
        return "no"
    end
end

local result : boolean
result = ispositive(1)
```

Oops -- we're returning string values, but we forgot to update the function return type. Since we've told Luau that ``ispositive()`` returns a boolean (and that's how we're using it), the call site isn't flagged as an error. But because the annotation doesn't match our code, we get a warning in the function body itself:

```
$ luau-analyze test.lua
test.lua(5,9): TypeError: Type 'string' could not be converted into 'boolean'
test.lua(7,9): TypeError: Type 'string' could not be converted into 'boolean'
```

The fix is simple; just change the annotation to declare the return type as a string:

```lua
--!strict

function ispositive(x : number) : string
    if x > 0 then
        return "yes"
    else
        return "no"
    end
end

local result : boolean
result = ispositive(1)
```

Well, almost - since we declared ``result`` as a boolean, the call site is now flagged:

```
$ luau-analyze test.lua
test.lua(12,10): TypeError: Type 'string' could not be converted into 'boolean'
```

If we update the type of the local variable, everything is good. Note that we could also just let Luau infer the type of ``result`` by changing it to the single line version ``local result = ispositive(1)``.

```lua
--!strict

function ispositive(x : number) : string
    if x > 0 then
        return "yes"
    else
        return "no"
    end
end

local result : string
result = ispositive(1)
```

## Conclusions

This has been a brief tour of the basic functionality of Luau, but there's lots more to explore. If you're interested in reading more, check out our main reference pages for [syntax](syntax) and [typechecking](typecheck).
