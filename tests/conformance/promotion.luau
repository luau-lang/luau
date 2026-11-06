-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing native code genearation auto-promotion and rebuild")

@native
function sumvectors(start, table)
    local sum = start

    for j = 1,1000 do
        for i, v in table do
            sum = sum + v
        end
    end

    return sum
end

-- function should switch to vectors
function test1(frame)
    local t = {}

    for j = 1, 100 do
        table.insert(t, vector.create(1, 2, 3))
    end

    local res = sumvectors(vector.create(0, 0, 0), t)
    assert(res == vector.create(100000, 200000, 300000))
end

@native
function sumvalues(start, table)
    local sum = start

    for j = 1,1000 do
        for i, v in table do
            sum = sum + v
        end
    end

    return sum
end

-- function is switches between numbers and vectors, so remains on the number-focused path
function test2(frame)
    local usevectors = frame == 1 or frame == 3

    local t = {}

    for j = 1, 100 do
        table.insert(t, if usevectors then vector.create(1, 2, 3) else 1)
    end

    local res = sumvalues(if usevectors then vector.create(0, 0, 0) else 0, t)

    if usevectors then
        assert(res == vector.create(100000, 200000, 300000))
    else
        assert(res == 100000)
    end
end

@native
function sumnumbers(start, table)
    local sum = start

    for j = 1,1000 do
        for i, v in table do
            sum = sum + v
        end
    end

    return sum
end

-- no rebuilds for expected path
function test3(frame)
    local t = {}

    for j = 1, 100 do
        table.insert(t, 1)
    end

    local res = sumnumbers(0, t)
    assert(res == 100000)
end

local Complex = {}

Complex.__add = function(a, b)
    return setmetatable({ re = a.re + b.re, im = a.im + b.im }, Complex)
end

Complex.__mul = function(a, b)
    return setmetatable({ re = a.re * b.re - a.im * b.im, im = a.re * b.im + a.im * b.re }, Complex)
end

@native
function iteratecomplex(c, count)
    local z = c

    for i = 1, count do
        z = z * z + c
    end

    return z
end

-- function is specialized for a generic case
function test4(frame)
    for j = 1, 1000 do
        local z = iteratecomplex(setmetatable({ re = 0, im = 0 }, Complex), 100)
        assert(z.re == 0 and z.im == 0)
    end
end

@native
function scalevectors(start, table, scale)
    local sum = start

    for j = 1,1000 do
        for i, v in table do
            sum = sum + v * scale
        end
    end

    return sum
end

-- function is specialized for a number/vector mixed operation
function test5(frame)
    local t = {}

    for j = 1, 100 do
        table.insert(t, vector.create(1, 2, 3))
    end

    local res = scalevectors(vector.create(0, 0, 0), t, 2)
    assert(res == vector.create(200000, 400000, 600000))
end

@native
function sumcomponents(table)
    local sum = 0

    for j = 1,1000 do
        for i, v in table do
            sum = sum + v.X + v.Y
        end
    end

    return sum
end

-- field access should switch to vectors
function test6(frame)
    local t = {}

    for j = 1, 100 do
        table.insert(t, vector.create(1, 2, 3))
    end

    local res = sumcomponents(t)
    assert(res == 300000)
end

@native
function vec2values(values)
    local sum = 0

    for j = 1,1000 do
        for _, v in values do
            sum += v.X + v:Dot(v)
        end
    end

    return sum
end

-- field access and method call should record the userdata tag and then switch to the host lowering of vec2
-- later, tables with the same field and method go through the fallback
function test7(frame)
    local values = {}

    for j = 1, 100 do
        table.insert(values, vec2(1, 2))
    end

    local expected = 1000 * 100 * (1 + 5)

    if frame >= 5 then
        table.insert(values, { X = 1, Dot = function(self, other) return 2 end })
        expected += 1000 * 3
    end

    local res = vec2values(values)
    assert(res == expected)
end

@native
function vec2operators(values)
    local sum = vec2(0, 0)

    for j = 1,1000 do
        for _, v in values do
            sum = sum + v * v + -v
        end
    end

    return sum
end

-- arithmetic should record the userdata tag and then switch to the host lowering of vec2
function test8(frame)
    local values = {}

    for j = 1, 100 do
        table.insert(values, vec2(1, 2))
    end

    -- each value adds (1 * 1 - 1, 2 * 2 - 2)
    local res = vec2operators(values)
    assert(res.X == 0 and res.Y == 1000 * 100 * 2)
end

@native
function userdatasizes(values)
    local sum = 0

    for j = 1,1000 do
        for _, v in values do
            sum += v.sizeof
        end
    end

    return sum
end

-- field access sees userdata with different tags, so it switches to the generic path
function test9(frame)
    local values = {}

    for j = 1, 50 do
        table.insert(values, vec2(1, 2))
        table.insert(values, vertex(vector.create(0, 0, 0), vector.create(0, 1, 0), vec2(1, 2)))
    end

    local expected = 1000 * 50 * (vec2(1, 2).sizeof + values[2].sizeof)

    local res = userdatasizes(values)
    assert(res == expected)
end

@native
function proxysizes(values)
    local sum = 0

    for j = 1,1000 do
        for _, v in values do
            sum += v.sizeof
        end
    end

    return sum
end

-- field access sees a userdata which doesn't map to a host type, so it switches to the generic path
function test10(frame)
    local proxy = newproxy(true)
    getmetatable(proxy).__index = function(self, key) return 4 end

    local values = {}

    for j = 1, 100 do
        table.insert(values, proxy)
    end

    local res = proxysizes(values)
    assert(res == 1000 * 100 * 4)
end

return 'OK'
