# Feature name

Goto statement

## Summary

goto statement for jumps and making scripting a bit easier.

## Motivation

### Problems

#### Breaking in nested loops
Let's say for example:
```lua
while true do
    for i=1, 100 do
        if (i == 10) then
            break -- This will only break the current for loop
        end
    end
end
```

In Luau there isn't a easy/simple way to break nested loops(without a variable)<br>

Current solution we have:
```lua
local RunLoop = true
while RunLoop do
    for i=1, 100 do
        if (i == 10) then
            RunLoop = false
            break
        end
    end
end
```
The problems that this solution has are:<br>
Messy: In a bigger scale this will get messy and confusing.<br>
Uneeded checks: Why do those checks when we could just jump out directly?




### Possible solution

A goto statement would make Luau more easy to script and cleaner, if used properly.<br>
```lua
while true do
    for i=1, 100 do
        if (i == 10) then
            goto LoopEnd         
        end
    end
end
:LoopEnd: -- Could follow Lua syntax(:LABEL:)
```


## Design



`:<ConstantString>:` Sets a label<br>
Notes:<br>
A label can't be used in another scope
```lua
function A()
    :FunctionStart:
        
    do
        if Y == Y then
            goto FunctionStart -- However, this would work
        end
    end

    if X == X then
        goto FunctionStart -- This would act the same as a while loop.
    end
end

function B()
    goto FunctionStart -- Undefined label compile error
end
```

`goto <ConstantString>` Jumps to a set label

## Drawbacks

Cross thread jumping could be a problem(but it would be very interesting if implemented and used correctly).<br>
Lambdas/Functions that are called could bypass the scope boundary without runtime checks.<br>

## Alternatives

A break for nested loops
```lua
while true do
    for i=1, 100 do
        if (i == 10) then
            :break -- The ':' means "break depth"
        end
    end
end
```
