-- tests for if-local syntax

-- basic truthy path
do
    if local x = 42 then
        assert(x == 42)
    else
        assert(false)
    end
end

-- falsy path (nil)
do
    local entered = false
    if local x = nil then
        entered = true
    end
    assert(not entered)
end

-- falsy path (false)
do
    local entered = false
    if local x = false then
        entered = true
    end
    assert(not entered)
end

-- else branch taken when falsy
do
    local result = "none"
    if local x = nil then
        result = "then"
    else
        result = "else"
    end
    assert(result == "else")
end

-- variable scoping: not visible after end
do
    local x = "outer"
    if local x = "inner" then
        assert(x == "inner")
    end
    assert(x == "outer")
end

-- variable scoping: not visible in else
do
    local x = "outer"
    if local x = nil then
        assert(false)
    else
        assert(x == "outer")
    end
end

-- elseif with independent bindings
do
    local result = ""
    if local x = nil then
        result = "first"
    elseif local y = "hello" then
        assert(y == "hello")
        result = "second"
    else
        result = "else"
    end
    assert(result == "second")
end

-- elseif falls through correctly
do
    local result = ""
    if local x = nil then
        result = "first"
    elseif local y = false then
        result = "second"
    else
        result = "else"
    end
    assert(result == "else")
end

-- if const basic
do
    if const x = 100 then
        assert(x == 100)
    end
end

-- truthy values: numbers, strings, tables
do
    if local x = 0 then
        assert(x == 0)
    else
        assert(false)
    end

    if local x = "" then
        assert(x == "")
    else
        assert(false)
    end

    local t = {}
    if local x = t then
        assert(x == t)
    else
        assert(false)
    end
end

-- function return value
do
    local function maybeGet(flag)
        if flag then
            return "found"
        end
        return nil
    end

    if local val = maybeGet(true) then
        assert(val == "found")
    end

    local entered = false
    if local val = maybeGet(false) then
        entered = true
    end
    assert(not entered)
end

-- nested if-local
do
    local function getUser()
        return { email = "test@example.com" }
    end

    if local user = getUser() then
        if local email = user.email then
            assert(email == "test@example.com")
        end
    end
end

-- shadowing outer variable
do
    local x = 10
    if local x = 20 then
        assert(x == 20)
    end
    assert(x == 10)
end

-- upvalue capture from then-block
do
    local captured
    if local x = "hello" then
        captured = function() return x end
    end
    assert(captured() == "hello")
end

-- pcall success: binds the success boolean (true), takes then-branch
do
    if local ok = pcall(function() end) then
        assert(ok == true)
    else
        assert(false)
    end
end

-- pcall failure: binds the success boolean (false), takes else-branch
do
    local result = "none"
    if local ok = pcall(function() error("boom") end) then
        result = "then"
    else
        result = "else"
    end
    assert(result == "else")
end

-- xpcall success: binds the success boolean (true)
do
    if local ok = xpcall(function() return 5 end, function() return "handled" end) then
        assert(ok == true)
    else
        assert(false)
    end
end

-- xpcall failure: handler runs, success boolean is false, takes else-branch
do
    local handlerRan = false
    local result = "none"
    if local ok = xpcall(function() error("boom") end, function() handlerRan = true return "handled" end) then
        result = "then"
    else
        result = "else"
    end
    assert(result == "else")
    assert(handlerRan)
end

-- multiple return values: the condition is a single-value binding, so only the first is kept
do
    local function multi()
        return 1, 2, 3
    end
    if local a = multi() then
        assert(a == 1)
    else
        assert(false)
    end
end

-- multiple return values whose first value is falsy: else-branch taken
do
    local function multiFalsy()
        return nil, "err"
    end
    local result = "none"
    if local a = multiFalsy() then
        result = "then"
    else
        result = "else"
    end
    assert(result == "else")
end

-- multireturn truncation is observable: a trailing truthy value must not keep the then-branch alive
do
    local function falseThenTrue()
        return false, true
    end
    local entered = false
    if local a = falseThenTrue() then
        entered = true
    end
    assert(not entered)
end

return "OK"
