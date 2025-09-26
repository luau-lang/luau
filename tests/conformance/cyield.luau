-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing yields from C functions')

-- Regular yield from a C function
do
    local co = coroutine.wrap(singleYield)
    assert(co() == 2)
    assert(co() == 4)
end

-- Multiple yields from a C function
do
    local co = coroutine.wrap(multipleYields)
    assert(co(10) == 11)
    assert(co() == 12)
    assert(co() == 13)
    assert(co() == 14)
end

-- Multiple yields from a C function (unused extra arguments)
do
    local co = coroutine.wrap(multipleYields)
    assert(co(10, -1) == 11)
    assert(co(-1, -2) == 12)
    assert(co(-1, -2) == 13)
    assert(co(-1, -2) == 14)
end

-- Multiple yields from nested yieldable C functions
do
    local co = coroutine.wrap(multipleYieldsWithNestedCall)
    assert(co(10, true) == 105)
    assert(co() == 115)
    assert(co() == 210)
    assert(co() == 220)
end

-- Multiple yields from nested yieldable C functions (unused extra arguments)
do
    local co = coroutine.wrap(multipleYieldsWithNestedCall)
    assert(co(10, true, -1) == 105)
    assert(co(-1, -2) == 115)
    assert(co(-1, -2) == 210)
    assert(co(-1, -2) == 220)
end

do
    local co = coroutine.wrap(multipleYieldsWithNestedCall)
    assert(co(10, false) == 110)
    assert(co() == 210)
    assert(co() == 220)
end

local function nonyieldable(x, y)
    return x / y
end

local function yieldable(x, y)
    local a, b = coroutine.yield(x + y)

    for i = 1,10 do
        a, b = coroutine.yield(a * b)
    end

    coroutine.yield(a - b)
    return x / y
end

-- yieldable Luau function that acts as a pass-through
do
    local co = coroutine.create(function(x, y)
        return yieldable(x, y)
    end)

    local s, x = coroutine.resume(co, 1, 2)
    assert(s)
    assert(x == 3)

    for i = 1, 10 do
        local s, x = coroutine.resume(co, 4, 8)
        assert(s)
        assert(x == 32)
    end

    local s, x = coroutine.resume(co, 10, 5)
    assert(s)
    assert(x == 5)
    local s, x = coroutine.resume(co)
    assert(s)
    assert(x == 0.5)
end

-- yieldable C function that acts as a pass-through (regular)
local function passthroughcheck(f)
    local x = f(nonyieldable, 1, 2)
    assert(x == 0.5)
end

passthroughcheck(passthroughCall)
passthroughcheck(passthroughCallMoreResults)
passthroughcheck(passthroughCallArgReuse)
passthroughcheck(passthroughCallVaradic)
passthroughcheck(passthroughCallWithState)

-- yieldable C function that acts as a pass-through (yield)
local function passthroughcheckyield(f)
    local co = coroutine.create(function(x, y)
        return f(yieldable, x, y)
    end)

    local s, x = coroutine.resume(co, 1, 2)
    assert(s)
    assert(x == 3)

    for i = 1, 10 do
        local s, x = coroutine.resume(co, 4, 8)
        assert(s)
        assert(x == 32)
    end

    local s, x = coroutine.resume(co, 10, 5)
    assert(s)
    assert(x == 5)
    local s, x = coroutine.resume(co)
    assert(s)
    assert(x == 0.5)
end

passthroughcheckyield(passthroughCall)
passthroughcheckyield(passthroughCallMoreResults)
passthroughcheckyield(passthroughCallArgReuse)
passthroughcheckyield(passthroughCallVaradic)
passthroughcheckyield(passthroughCallWithState)

-- yieldable C function that acts as a pass-through (regular, pcall to Luau)
local function nonyieldablepcall(x, y)
    local status, result = pcall(nonyieldable, x, y)
    assert(status)
    return result
end

local function passthroughcheckpcall(f)
    local x = f(nonyieldablepcall, 1, 2)
    assert(x == 0.5)
end

passthroughcheckpcall(passthroughCall)
passthroughcheckpcall(passthroughCallMoreResults)
passthroughcheckpcall(passthroughCallArgReuse)
passthroughcheckpcall(passthroughCallVaradic)
passthroughcheckpcall(passthroughCallWithState)

-- yieldable C function that acts as a pass-through (regular, pcall to C)
local function nonyieldablepcallc(x, y)
    local status, result = pcall(assert, x / y)
    assert(status)
    return result
end

local function passthroughcheckpcallc(f)
    local x = f(nonyieldablepcallc, 1, 2)
    assert(x == 0.5)
end

passthroughcheckpcallc(passthroughCall)
passthroughcheckpcallc(passthroughCallMoreResults)
passthroughcheckpcallc(passthroughCallArgReuse)
passthroughcheckpcallc(passthroughCallVaradic)
passthroughcheckpcallc(passthroughCallWithState)

-- yieldable C function that acts as a pass-through (regular, direct pcall)
local function passthroughcheckpcallcdirect(f)
    local status, x = f(pcall, function(y) return 1 / y end, 2)
    assert(status)
    assert(x == 0.5)
end

-- only variadic functions are tested here because pcall returns status as well
passthroughcheckpcallcdirect(passthroughCallVaradic)
passthroughcheckpcallcdirect(passthroughCallWithState)

return "OK"
