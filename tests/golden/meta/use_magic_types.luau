local function foo()
    local x: number = 5
    local y: string = "hello"
    local z: boolean = true
    return x, y, z
end

local bar: _luau_print<typeof(foo)> = foo
