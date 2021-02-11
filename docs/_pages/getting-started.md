---
permalink: /getting-started
title: Getting Started
toc: true
---

To get started with Luau you need to install Roblox Studio, which you can download [here](https://www.roblox.com/create). 

## Creating a place

If you just want to experiment with the language itself, you can create a simple baseplate game.

<figure>
  <img src="/assets/images/create-new-place.png">
</figure>

## Creating a script

To create your own testing script, go to ServerScriptService in the explorer tree and add a Script object.

<figure>
  <img src="/assets/images/create-script.png">
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

## Enabling type inference

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

Note that the editor now highlights the incorrect calls to ``ispositive()`` and ``isfoo()``.

<figure>
  <img src="/assets/images/error-ispositive.png">
  <img src="/assets/images/error-isfoo.png">
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

Everything is good. We've told the editor that ``ispositive()`` accepts a number and returns a boolean, and that's how we're using it. But imagine that later we decide to change ``ispositive()`` to return a string value:

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

Oops -- we're returning string values, but we forgot to update the function return type. Since ``print()`` accepts anything, the call to ``ispositive()`` is still valid. But because the annotation doesn't match our code, we get a warning in the function body itself:

<figure>
  <img src="/assets/images/error-ispositive-string.png">
</figure>

And then of course, the fix is simple; just change the annotation to declare the return type as a string.

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

Well, almost - we also declared result as a boolean, and now that's clearly wrong.

<figure>
  <img src="/assets/images/error-ispositive-boolean.png">
</figure>

So now we update the type of the local variable, and everything is good.

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

To dive into more areas of Luau, check out our main reference pages for [syntax](syntax) and [typechecking](typecheck)
