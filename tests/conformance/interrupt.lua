-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing interrupts")

function foo()
    for i=1,10 do end
    return
end

foo()

function bar()
    local i = 0
    while i < 10 do
        i += i + 1
    end
end

bar()

return "OK"
