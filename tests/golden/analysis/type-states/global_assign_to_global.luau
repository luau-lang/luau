A = 42
B = A
C = B

local function printit()
    print("A = " .. A)
    print("B = " .. B)
    print("C = " .. B)
end

local _: _luau_print<typeof(A)>
local _: _luau_print<typeof(B)>
local _: _luau_print<typeof(C)>

printit()