-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing vector library')

-- detect vector size
local vector_size = if pcall(function() return vector(0, 0, 0).w end) then 4 else 3

function ecall(fn, ...)
    local ok, err = pcall(fn, ...)
    assert(not ok)
    return err:sub((err:find(": ") or -1) + 2, #err)
end

-- make sure we cover both builtin and C impl
assert(vector.create(1, 2) == vector.create("1", "2"))
assert(vector.create(1, 2, 4) == vector.create("1", "2", "4"))

-- 'create'
local v12 = vector.create(1, 2)
local v123 = vector.create(1, 2, 3)
assert(v12.x == 1 and v12.y == 2 and v12.z == 0)
assert(v123.x == 1 and v123.y == 2 and v123.z == 3)

-- testing 'dot' with error handling and different call kinds to mostly check details in the codegen
assert(vector.dot(vector.create(1, 2, 4), vector.create(5, 6, 7)) == 45)
assert(ecall(function() vector.dot(vector.create(1, 2, 4)) end) == "missing argument #2 to 'dot' (vector expected)")
assert(ecall(function() vector.dot(vector.create(1, 2, 4), 2) end) == "invalid argument #2 to 'dot' (vector expected, got number)")

local function doDot1(a: vector, b)
    return vector.dot(a, b)
end

local function doDot2(a: vector, b)
    return (vector.dot(a, b))
end

local v124 = vector.create(1, 2, 4)

assert(doDot1(v124, vector.create(5, 6, 7)) == 45)
assert(doDot2(v124, vector.create(5, 6, 7)) == 45)
assert(ecall(function() doDot1(v124, "a") end) == "invalid argument #2 to 'dot' (vector expected, got string)")
assert(ecall(function() doDot2(v124, "a") end) == "invalid argument #2 to 'dot' (vector expected, got string)")
assert(select("#", doDot1(v124, vector.create(5, 6, 7))) == 1)
assert(select("#", doDot2(v124, vector.create(5, 6, 7))) == 1)

-- 'cross' tests and next ones will only test basic results
assert(vector.cross(vector.create(1, 0, 0), vector.create(0, 1, 0)) == vector.create(0, 0, 1))
assert(vector.cross(vector.create(0, 1, 0), vector.create(1, 0, 0)) == vector.create(0, 0, -1))
assert(select("#", vector.cross(vector.zero, vector.one)) == 1)

-- 'normalize'
assert(vector.normalize(vector.create(0.5, 0, 0)) == vector.create(1, 0, 0))
assert(select("#", vector.normalize(vector.one)) == 1)

-- 'magnitude'
assert(vector.magnitude(vector.create(1, 2, 2)) == 3)
assert(select("#", vector.magnitude(vector.one)) == 1)

-- 'abs'
assert(vector.abs(-vector.one) == vector.one)
assert(vector.abs(vector.create(math.huge, 0, 0)).x == math.abs(math.huge))
assert(vector.abs(vector.create(0/0, 0, 0)).x ~= 0/0)
assert(select("#", vector.abs(vector.one)) == 1)

-- 'floor'
assert(vector.floor(vector.create(1, 2, 3)) == vector.create(1, 2, 3))
assert(vector.floor(vector.create(1.5, 2.4, 3)) == vector.create(1, 2, 3))
assert(vector.floor(vector.create(-1.5, -2.4, -3)) == vector.create(-2, -3, -3))
assert(select("#", vector.floor(vector.one)) == 1)

-- 'ceil'
assert(vector.ceil(vector.create(1, 2, 3)) == vector.create(1, 2, 3))
assert(vector.ceil(vector.create(1.5, 2.4, 3)) == vector.create(2, 3, 3))
assert(vector.ceil(vector.create(-1.5, -2.4, -3)) == vector.create(-1, -2, -3))
assert(select("#", vector.ceil(vector.one)) == 1)

-- 'sign'
assert(vector.sign(vector.zero) == vector.zero)
assert(vector.sign(vector.one) == vector.one)
assert(vector.sign(vector.create(-10, 0, 10)) == vector.create(-1, 0, 1))
assert(vector.sign(vector.create(math.huge, 0, -math.huge)) == vector.create(1, 0, -1))
-- negative zero and nan are consistent with math library, even if implementation defined
assert(vector.sign(vector.create(-0, 0, 0)).x == math.sign(-0))
assert(vector.sign(vector.create(0/0, 0, 0)).x == math.sign(0/0))
assert(select("#", vector.sign(vector.one)) == 1)

-- 'angle'
assert(math.abs(vector.angle(vector.create(1, 2, 3), vector.create(4, 5, 6)) - 0.2257259) < 0.00001)
assert(select("#", vector.angle(vector.zero, vector.one)) == 1)
assert(select("#", vector.angle(vector.one, -vector.one, vector.zero)) == 1)

do
    -- random (non-unit) vectors
    local rand = {
        vector.create(-1.05, -0.04, 1.06),
        vector.create(-0.75, 1.71, 1.29),
        vector.create(1.94, 0.76, -0.93),
        vector.create(0.02, -1.58, 0.20),
        vector.create(1.64, -0.76, -0.73),
        vector.create(-2.44, 0.66, 1.06),
        vector.create(-2.61, 1.01, 0.50),
        vector.create(1.21, -2.28, -0.45),
        vector.create(-0.31, -0.12, 1.96),
        vector.create(1.16, -0.07, -1.93)
    }

    -- numeric answers to the tests below (in degrees)
    local ans = {
        -105.1702,
        -69.49491,
        0.0,
        -102.9083,
        0.0,
        0.0,
        180.0,
        -0.02797646,
        -90.0,
        165.8858
    }

    for i,v in ans do
        ans[i] = math.rad(ans[i])
    end
    
    local function fuzzyeq(x, y, eps) return x == y or math.abs(x - y) < (eps or 1e-6) end

    assert(fuzzyeq(vector.angle(rand[10], rand[1]), math.abs(ans[10])))
    assert(fuzzyeq(vector.angle(rand[2], rand[3]), math.abs(ans[1])))
    assert(fuzzyeq(vector.angle(rand[4], rand[5]), math.abs(ans[2])))
    assert(fuzzyeq(vector.angle(vector.zero, rand[6]), math.abs(ans[3])))
    assert(fuzzyeq(vector.angle(vector.one, rand[7]), math.abs(ans[4])))
    assert(fuzzyeq(vector.angle(vector.zero, vector.zero), math.abs(ans[5])))
    assert(fuzzyeq(vector.angle(rand[8], rand[8]), math.abs(ans[6])))
    assert(fuzzyeq(vector.angle(-rand[8], rand[8]), math.abs(ans[7])))
    assert(fuzzyeq(vector.angle(rand[9], rand[9] + vector.create(0, 1, 0) * 0.001), math.abs(ans[8]), 1e-3)) -- slightly more generous eps
    assert(fuzzyeq(vector.angle(vector.create(1, 0, 0), vector.create(0, 1, 0)), math.abs(ans[9])))

    assert(fuzzyeq(vector.angle(rand[10], rand[1], rand[2]), ans[10]))
    assert(fuzzyeq(vector.angle(rand[2], rand[3], rand[4]), ans[1]))
    assert(fuzzyeq(vector.angle(rand[4], rand[5], rand[5]), ans[2]))
    assert(fuzzyeq(vector.angle(vector.zero, rand[6], rand[10]), ans[3]))
    assert(fuzzyeq(vector.angle(vector.one, rand[7], rand[10]), ans[4]))
    assert(fuzzyeq(vector.angle(vector.zero, vector.zero, vector.zero), ans[5]))
    assert(fuzzyeq(vector.angle(rand[8], rand[8], rand[10]), ans[6]))
    assert(fuzzyeq(vector.angle(rand[9], rand[9] + vector.create(0, 1, 0) * 0.001, rand[10]), ans[8], 1e-3)) -- slightly more generous eps
    assert(fuzzyeq(vector.angle(vector.create(1, 0, 0), vector.create(0, 1, 0), rand[10]), ans[9]))
end

-- 'min'/'max'
assert(vector.max(vector.create(-1, 2, 0.5)) == vector.create(-1, 2, 0.5))
assert(vector.min(vector.create(-1, 2, 0.5)) == vector.create(-1, 2, 0.5))

assert(ecall(function() vector.min() end) == "missing argument #1 to 'min' (vector expected)")
assert(ecall(function() vector.max() end) == "missing argument #1 to 'max' (vector expected)")

assert(select("#", vector.max(vector.zero, vector.one)) == 1)
assert(select("#", vector.min(vector.zero, vector.one)) == 1)

assert(vector.max(vector.create(-1, 2, 3), vector.create(3, 2, 1)) == vector.create(3, 2, 3))
assert(vector.min(vector.create(-1, 2, 3), vector.create(3, 2, 1)) == vector.create(-1, 2, 1))

assert(vector.max(vector.create(1, 2, 3),vector.create(2, 3, 4),vector.create(3, 4, 5),vector.create(4, 5, 6)) == vector.create(4, 5, 6))
assert(vector.min(vector.create(1, 2, 3),vector.create(2, 3, 4),vector.create(3, 4, 5),vector.create(4, 5, 6)) == vector.create(1, 2, 3))

-- clamp
assert(vector.clamp(vector.create(1, 1, 1), vector.create(0, 1, 2), vector.create(3, 3, 3)) == vector.create(1, 1, 2))
assert(vector.clamp(vector.create(1, 1, 1), vector.create(-1, -1, -1), vector.create(0, 1, 2)) == vector.create(0, 1, 1))
assert(select("#", vector.clamp(vector.zero, vector.zero, vector.one)) == 1)

-- validate component access
assert(vector.create(1, 2, 3).x == 1)
assert(vector.create(1, 2, 3).X == 1)
assert(vector.create(1, 2, 3)['X'] == 1)
assert(vector.create(1, 2, 3).y == 2)
assert(vector.create(1, 2, 3).Y == 2)
assert(vector.create(1, 2, 3)['Y'] == 2)
assert(vector.create(1, 2, 3).z == 3)
assert(vector.create(1, 2, 3).Z == 3)
assert(vector.create(1, 2, 3)['Z'] == 3)

local function getcomp(v: vector, field: string)
    return v[field]
end

assert(getcomp(vector.create(1, 2, 3), 'x') == 1)
assert(getcomp(vector.create(1, 2, 3), 'y') == 2)
assert(getcomp(vector.create(1, 2, 3), 'z') == 3)

assert(ecall(function() return vector.create(1, 2, 3).zz end) == "attempt to index vector with 'zz'")

-- additional checks for 4-component vectors
if vector_size == 4 then
	assert(vector.create(1, 2, 3, 4).w == 4)
	assert(vector.create(1, 2, 3, 4).W == 4)
	assert(vector.create(1, 2, 3, 4)['W'] == 4)
end

return 'OK'
