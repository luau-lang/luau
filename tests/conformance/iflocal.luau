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

-- if const: falsy path takes else branch
do
    local result = "none"
    if const x = nil then
        result = "then"
    else
        result = "else"
    end
    assert(result == "else")
end

-- if const: variable scoped to then-block only
do
    local x = "outer"
    if const x = "inner" then
        assert(x == "inner")
    end
    assert(x == "outer")
end

-- elseif const with independent binding
do
    local result = ""
    if local x = nil then
        result = "first"
    elseif const y = "hey" then
        assert(y == "hey")
        result = "second"
    end
    assert(result == "second")
end

-- initializer is evaluated exactly once
do
    local calls = 0
    local function sideEffect()
        calls += 1
        return calls
    end
    if local x = sideEffect() then
        assert(x == 1)
    end
    assert(calls == 1)
end

-- initializer evaluated once even when falsy (else branch taken)
do
    local calls = 0
    local function sideEffectNil()
        calls += 1
        return nil
    end
    local entered = false
    if local x = sideEffectNil() then
        entered = true
    end
    assert(not entered)
    assert(calls == 1)
end

-- break out of a loop from an if-local then-block
do
    local seen = {}
    for i = 1, 5 do
        if local x = i then
            table.insert(seen, x)
            if x == 3 then
                break
            end
        end
    end
    assert(#seen == 3)
    assert(seen[3] == 3)
end

-- continue in a loop from an if-local then-block
do
    local sum = 0
    for i = 1, 5 do
        if local even = (i % 2 == 0) then
            continue
        end
        sum += i
    end
    assert(sum == 1 + 3 + 5)
end

-- nested if-local, inner binding shadows nothing and is independent
do
    local function getUser()
        return { token = { id = 42 } }
    end
    local found
    if local user = getUser() then
        if local token = user.token then
            found = token.id
        end
    end
    assert(found == 42)
end

-- each loop iteration gets a fresh binding captured independently
do
    local closures = {}
    for i = 1, 3 do
        if local x = i then
            closures[i] = function() return x end
        end
    end
    assert(closures[1]() == 1)
    assert(closures[2]() == 2)
    assert(closures[3]() == 3)
end

-- return from within the then-block
do
    local function f(v)
        if local x = v then
            return x
        end
        return "fallthrough"
    end
    assert(f(7) == 7)
    assert(f(nil) == "fallthrough")
end

return "OK"
