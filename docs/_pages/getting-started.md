---
permalink: /getting-started
title: Getting Started
toc: true
---

To get started with Luau you need to install Roblox Studio, which you can download [here](https://www.roblox.com/create). 

## Creating a place

If you just want to experiment with the language itself, you can create a simple baseplate game.

<figure>
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/create-new-place.png">
</figure>

## Creating a script

To create your own testing script, go to ServerScriptService in the explorer tree and add a Script object.

<figure>
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/create-script.png">
</figure>

Double-click on the script object and paste this:

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

Note that there are no warnings about calling ``ispositive()`` with a string, or calling ``isfoo()`` a number. 

## Type inference

Now modify the script to include ``--!strict`` at the top:

```lua
--!strict

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

In ``strict`` mode, Luau will infer types based on analysis of the code flow. There is also ``nonstrict`` mode, where analysis is more conservative and types are more frequently inferred as ``any`` to reduce cases where legitimate code is flagged with warnings.

In this case, Luau will use the ``return x > 0`` statement to infer that ``ispositive()`` is a function taking an integer and returning a boolean. Similarly, it will use the ``return a == "foo"`` statement to infer that ``isfoo()`` is a function taking a string and returning a boolean. Note that in both cases, it was not necessary to add any explicit type annotations.

Based on Luau's type inference, the editor now highlights the incorrect calls to ``ispositive()`` and ``isfoo()``:

<figure>
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/error-ispositive.png">
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/error-isfoo.png">
</figure>

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

<figure>
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/error-ispositive-string.png">
</figure>

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

<figure>
  <img src="{{ site.url }}{{ site.baseurl }}/assets/images/error-ispositive-boolean.png">
</figure>

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
