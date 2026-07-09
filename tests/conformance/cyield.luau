-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing yields from C functions')

function ecall(fn, ...)
    local ok, err = pcall(fn, ...)
    print(ok, err)
    assert(not ok)
    return err:sub(err:find(": ") + 2, #err)
end

function resume(co, ...)
    local ok, result = coroutine.resume(co)
    print(ok, result)
    assert(ok)
    return result
end

function eresume(co, ...)
    local ok, err = coroutine.resume(co)
    print(ok, err)
    assert(not ok)
    return err:sub(err:find(": ") + 2, #err)
end

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

-- test that error throws correctly restore C call counter
if not limitedstack then
	local function errf()
		error("boom")
	end

	for i = 1, 100 do
		local ok, err = pcall(function() passthroughCallVaradic(errf) end)
		assert(not ok)
		assert(string.find(err, "boom"), `iter {i}: expected boom, got {err}`)
	end

	local count = 0
	local function foo()
		count += 1
		pcall(1)
		coroutine.wrap(foo)()
	end
	local ok2 = pcall(foo)
	assert(not ok2)
	assert(count == 200, `expected count == 200 (MAXCCALLS), got {count}`)
end

local function ok_simple()
    return 100
end

local function err_simple()
    error("error_sync")
end

local function yield_then_ok()
    coroutine.yield(15)
    return 5
end

local function yield_then_err()
    coroutine.yield(25)
    error("error_async")
end

assert(pcallThenCall(ok_simple, ok_simple) == 100 * 100)
assert(ecall(pcallThenCall, ok_simple, err_simple) == "error_sync")

assert(pcallThenCall(err_simple, ok_simple) == -100)
assert(ecall(pcallThenCall, err_simple, err_simple) == "error_sync")

do
    local co = coroutine.create(function() return pcallThenCall(ok_simple, yield_then_ok) end)
    
    assert(resume(co) == 15)
    assert(resume(co) == 100 * 5)
end

do
    local co = coroutine.create(function() return pcallThenCall(ok_simple, yield_then_err) end)
    
    assert(resume(co) == 25)
    assert(eresume(co) == "error_async")
end

do
    local co = coroutine.create(function() return pcallThenCall(yield_then_ok, ok_simple) end)

    assert(resume(co) == 15)
    assert(resume(co) == 5 * 100)
end

do
    local co = coroutine.create(function() return pcallThenCall(yield_then_ok, err_simple) end)
    
    assert(resume(co) == 15)
    assert(eresume(co) == "error_sync")
end

do
    local co = coroutine.create(function() return pcallThenCall(yield_then_ok, yield_then_ok) end)
    
    assert(resume(co) == 15)
    assert(resume(co) == 15)
    assert(resume(co) == 5 * 5)
end

do
    local co = coroutine.create(function() return pcallThenCall(yield_then_ok, yield_then_err) end)

    assert(resume(co) == 15)
    assert(resume(co) == 25)
    assert(eresume(co) == "error_async")
end

do
    local co = coroutine.create(function() return pcallThenCall(yield_then_err, yield_then_ok) end)

    assert(resume(co) == 25)
    assert(resume(co) == 15)
    assert(resume(co) == -1 * 5)
end

do
    local co = coroutine.create(function() return pcallThenCall(err_simple, yield_then_ok) end)

    assert(resume(co) == 15)
    assert(resume(co) == -1 * 5)
end

-- extra tests where second call in continuation is also protected
do
    local co = coroutine.create(function() return pcallThenPcall(yield_then_ok, ok_simple) end)

    assert(resume(co) == 15)
    assert(resume(co) == 5 * 100)
end

assert(pcallThenPcall(ok_simple, err_simple) == 100 * -1)

return "OK"
