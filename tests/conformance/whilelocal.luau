-- tests for while-local syntax

-- basic loop: binding re-evaluated each iteration, exits when falsy
do
    local counter = 3
    local seen = {}
    local function next()
        counter = counter - 1
        if counter < 0 then
            return nil
        end
        return counter
    end

    while local x = next() do
        table.insert(seen, x)
    end

    assert(#seen == 3)
    assert(seen[1] == 2)
    assert(seen[2] == 1)
    assert(seen[3] == 0)
end

-- loop that never runs: condition falsy on first evaluation (nil)
do
    local entered = false
    while local x = nil do
        entered = true
    end
    assert(not entered)
end

-- loop that never runs: condition falsy on first evaluation (false)
do
    local entered = false
    while local x = false do
        entered = true
    end
    assert(not entered)
end

-- binding is truthy inside the body even when 0 (0 is truthy in Luau)
do
    local iterations = 0
    local limit = 2
    while local n = (iterations < limit) and iterations or false do
        assert(type(n) == "number")
        iterations = iterations + 1
    end
    assert(iterations == 2)
end

-- scoping: binding not visible after the loop ends
do
    local x = "outer"
    local ran = false
    while local x = (not ran) and "inner" or false do
        assert(x == "inner")
        ran = true
    end
    assert(x == "outer")
end

-- scoping: binding not visible in its own condition (reads the outer x each iteration)
do
    local x = 2
    local count = 0
    while local y = x > 0 do
        assert(y == true)
        x = x - 1
        count = count + 1
        if count > 10 then
            error("loop did not terminate")
        end
    end
    assert(count == 2)
end

-- break exits the loop
do
    local count = 0
    while local x = true do
        count = count + 1
        if count == 3 then
            break
        end
    end
    assert(count == 3)
end

-- continue skips to the next iteration (and re-evaluates the condition)
do
    local counter = 5
    local sum = 0
    local function next()
        counter = counter - 1
        if counter < 0 then
            return nil
        end
        return counter
    end

    while local x = next() do
        if x % 2 == 0 then
            continue
        end
        sum = sum + x
    end
    -- odd values produced by next(): 3, 1 => sum 4
    assert(sum == 4)
end

-- each iteration captures a fresh binding in a closure
do
    local closures = {}
    local counter = 3
    local function next()
        counter = counter - 1
        if counter < 0 then
            return nil
        end
        return counter
    end

    while local x = next() do
        table.insert(closures, function()
            return x
        end)
    end

    assert(#closures == 3)
    assert(closures[1]() == 2)
    assert(closures[2]() == 1)
    assert(closures[3]() == 0)
end

-- nested while-local loops with independent bindings
do
    local total = 0
    local i = 2
    local function nextI()
        i = i - 1
        if i < 0 then
            return nil
        end
        return i
    end

    while local a = nextI() do
        local j = 2
        local function nextJ()
            j = j - 1
            if j < 0 then
                return nil
            end
            return j
        end
        while local b = nextJ() do
            total = total + a + b
        end
    end
    -- a in {1,0}, b in {1,0} for each a => (1+1)+(1+0)+(0+1)+(0+0) = 4
    assert(total == 4)
end

-- mutating the binding inside the body does not affect the next iteration's condition
do
    local counter = 3
    local observed = {}
    local function next()
        counter = counter - 1
        if counter < 0 then
            return nil
        end
        return counter
    end

    while local x = next() do
        table.insert(observed, x)
        x = 999
    end
    assert(#observed == 3)
    assert(observed[1] == 2)
end

-- while const basic
do
    local counter = 3
    local seen = {}
    local function next()
        counter = counter - 1
        if counter < 0 then
            return nil
        end
        return counter
    end

    while const x = next() do
        table.insert(seen, x)
    end

    assert(#seen == 3)
    assert(seen[1] == 2)
    assert(seen[2] == 1)
    assert(seen[3] == 0)
end

-- while const: loop that never runs when condition is falsy on first evaluation
do
    local entered = false
    while const x = nil do
        entered = true
    end
    assert(not entered)
end

-- while const: binding scoped to the loop body only
do
    local x = "outer"
    local ran = false
    while const x = (not ran) and "inner" or false do
        assert(x == "inner")
        ran = true
    end
    assert(x == "outer")
end

-- backwards compatibility for context-sensitive const: `const` is still a usable identifier
do
    local const = 2
    local count = 0
    while const > 0 do
        const = const - 1
        count = count + 1
    end
    assert(count == 2)
end

return "OK"
