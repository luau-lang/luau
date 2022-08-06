# Table Destructuring

## Summary
A feature present and highly-used in JavaScript, table destructuring allows the developer to extract data from tables and assign them to variables in a clean, shortened manner, thus avoiding the need for excessive index calls and several individual assignments, most of which being expendable.


## Motivation
As of today, the only way to "dynamically" assign table values is via `unpack`/`select` and variable list assignments. With that being said, these functions cannot be used on dictionaries and limit control over what values are being retrieved.

With the introduction of destructuring assignments, we'll be able to substantially reduce redundant variables, index calls, and bloat in our code. Instead, using a clean syntax that allows the intuitive selection and assignment of values from arrays and dictionaries. 

Destructuring allows for shortened code with increased flexibility. The expected outcome__s__ are endless.

```lua
-- Example using HTTP Service
-- Based on example from https://developer.roblox.com/en-us/api-reference/class/HttpService

local response = HttpSevice:GetAsync("http://api.open-notify.org/astros.json")
local data = HttpService:JSONDecode(response)

local { message, number, people } = data

if message == "success" then
    print("There are currently", data.number, "astronauts in space:")
    for i, { name, craft } in people do
        print(i .. ": " .. name .. " is on " .. craft)
    end
end
```
```lua
-- Example using Fusion

local { New, Children } = Fusion

return New "ScreenGui" {
    -- ...
}
```
```lua
-- Example using a RemoteFunction callback

RemoteFunction.InvokeServer = function(player, { target, damage })
    print("Dealt " .. damage .. " to " .. target.Name)
end
```

## Design

### Variable Assignments for Arrays
With the ability to use `unpack` to destructure arrays, the need for this feature can seem negligible. Nevertheless, `unpack` cannot be used on dictionaries.
```lua
-- Current
local foo = {1, 2, 3}

local one, two, three = unpack(foo) -- 1, 2, 3
```
```lua
-- New
local foo = {1, 2, 3}

local {one, two, three} = foo -- 1, 2, 3
```

### Variable Assignments for Dictionaries
```lua
-- Current
local foo = {
    red = "good";
    green = "better";
    blue = "best";
}

local red, green, blue = foo.red, foo.green, foo.blue
print(red) -- good
print(green) -- better
print(blue) -- best
```
```lua
-- New
local foo = {
    red = "good";
    green = "better";
    blue = "best";
}

local {red, green, blue} = foo
print(red) -- good
print(green) -- better
print(blue) -- best
```

### Variable Assignments for Function Returns
```lua
-- Current
local function f()
    return {
        name = "John";
        age = 25;
        gender = "male";
    }
end

local data = f()
local name, age, gender = data.name, data.age, data.gender

print(name) -- John
print(age) -- 25
print(gender) -- male
```
```lua
-- New
local function f()
    return {
        name = "John";
        age = 25;
        gender = "male";
    }
end

local {name, age, gender} = f()
print(name) -- John
print(age) -- 25
print(gender) -- male
```

### Variable Assignments for UserData & Vectors
```lua
-- Current
local Part = workspace.Part
local Position = Part.CFrame.Position
local X, Y, Z = Position.X, Position.Y, Position.Z

print(X, Y, Z) -- 0, 0, 0
```
```lua
-- New
local Part = workspace.Part
local {Position} = Part.CFrame
local {X, Y, Z} = Position

print(X, Y, Z) -- 0, 0, 0
```

### Variable Assignments for Indexing Children (???)
```lua
-- Current
local Baseplate = workspace.Baseplate
print(Baseplate) -- Baseplate
```
```lua
-- New
local {Baseplate} = workspace
print(Baseplate) -- Baseplate
```

---

### Destructuring Function Parameters
```lua
-- Before
local function f(data)
    local name = data.name
    local age = data.age
    local gender = data.gender

    print(name, age, gender) -- John, 25, male
end

f({
    name = "John";
    age = 25;
    gender = "male";
})
```
```lua
-- New
local function f({ name, age, gender })
    print(name, age, gender) -- John, 25, male
end

f({
    name = "John";
    age = 25;
    gender = "male";
})
```

---

### Defining Alternative Identifiers (Questionable?)
In this example, `fizz-buzz` is not a valid identifier. You should be able to instead define an alternative identifier, like so.
```lua
local foo = {
    ["fizz-buzz"] = true;
}

local { fizzbuzz = "fizz-buzz" } = foo
print(fizzbuzz) -- fizz-buzz
```

### More Complex Example
This example is a direct port of the JavaScript example I gave on issue #617.
```lua
local obj = {
    name = "John";
    records = {
        {
            date = "7/29/2022";
            text = "Hello World!";
        }
    }
}

local {
    name,
    records = {
        { text }
    }
} = obj

print(name .. " says " .. text) -- John says Hello World!
```

## Drawbacks

From my perspective, this would be a rather large addition to the language. *Some* of the drawbacks are listed below:
* Increased compiler complexity
* Considered language bloat
* New, unknown syntax
* Not necessarily friendly to new programmers

With that being said, these drawbacks are all subjective, and likely won't matter to people who choose not to use destructuring in their code.

## Alternatives

The only other alternatives are `unpack`/`select`. If this weren't implemented, it would just lead to longer code.
