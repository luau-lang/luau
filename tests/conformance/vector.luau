-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing vectors')

-- detect vector size
local vector_size = if pcall(function() return vector(0, 0, 0).w end) then 4 else 3

function ecall(fn, ...)
	local ok, err = pcall(fn, ...)
	assert(not ok)
	return err:sub((err:find(": ") or -1) + 2, #err)
end

-- equality
assert(vector(1, 2, 3) == vector(1, 2, 3))
assert(vector(0, 1, 2) == vector(-0, 1, 2))
assert(vector(1, 2, 3) ~= vector(1, 2, 4))

-- rawequal
assert(rawequal(vector(1, 2, 3), vector(1, 2, 3)))
assert(rawequal(vector(0, 1, 2), vector(-0, 1, 2)))
assert(not rawequal(vector(1, 2, 3), vector(1, 2, 4)))

-- type & tostring
assert(type(vector(1, 2, 3)) == "vector")

if vector_size == 4 then
	assert(tostring(vector(1, 2, 3, 4)) == "1, 2, 3, 4")
	assert(tostring(vector(-1, 2, 0.5, 0)) == "-1, 2, 0.5, 0")
else
	assert(tostring(vector(1, 2, 3)) == "1, 2, 3")
	assert(tostring(vector(-1, 2, 0.5)) == "-1, 2, 0.5")
end

local t = {}

-- basic table access
t[vector(1, 2, 3)] = 42
assert(t[vector(1, 2, 3)] == 42)
assert(t[vector(1, 2, 4)] == nil)

-- negative zero should hash the same as zero
assert(t[vector(0, 0, 0)] == nil)
t[vector(0, 0, 0)] = "hello"
assert(t[vector(0, 0, 0)] == "hello")
assert(t[vector(0, -0, 0)] == "hello")

-- test arithmetic instructions
assert(vector(1, 2, 4) + vector(8, 16, 24) == vector(9, 18, 28));
assert(vector(1, 2, 4) - vector(8, 16, 24) == vector(-7, -14, -20));

local val = 1/'8'

assert(vector(1, 2, 4) * vector(8, 16, 24) == vector(8, 32, 96));
assert(vector(1, 2, 4) * 8 == vector(8, 16, 32));
assert(vector(1, 2, 4) * (1 / val) == vector(8, 16, 32));
assert(8 * vector(8, 16, 24) == vector(64, 128, 192));
assert(vector(1, 2, 4) * '8' == vector(8, 16, 32));
assert('8' * vector(8, 16, 24) == vector(64, 128, 192));

assert(vector(1, 2, 4) * -0.125 == vector(-0.125, -0.25, -0.5))
assert(-0.125 * vector(1, 2, 4) == vector(-0.125, -0.25, -0.5))

assert(vector(1, 2, 4) * 100 == vector(100, 200, 400))
assert(100 * vector(1, 2, 4) == vector(100, 200, 400))

if vector_size == 4 then
	assert(vector(1, 2, 4, 8) / vector(8, 16, 24, 32) == vector(1/8, 2/16, 4/24, 8/32));
	assert(8 / vector(8, 16, 24, 32) == vector(1, 1/2, 1/3, 1/4));
	assert('8' / vector(8, 16, 24, 32) == vector(1, 1/2, 1/3, 1/4));
else
	assert(vector(1, 2, 4) / vector(8, 16, 24, 1) == vector(1/8, 2/16, 4/24));
	assert(8 / vector(8, 16, 24) == vector(1, 1/2, 1/3));
	assert('8' / vector(8, 16, 24) == vector(1, 1/2, 1/3));
end

assert(vector(1, 2, 4) / 8 == vector(1/8, 1/4, 1/2));
assert(vector(1, 2, 4) / (1 / val) == vector(1/8, 2/8, 4/8));
assert(vector(1, 2, 4) / '8' == vector(1/8, 1/4, 1/2));

assert(-vector(1, 2, 4) == vector(-1, -2, -4));

-- test floor division
assert(vector(1, 3, 5) // 2 == vector(0, 1, 2))
assert(vector(1, 3, 5) // val == vector(8, 24, 40))

if vector_size == 4 then
	assert(10 // vector(1, 2, 3, 4) == vector(10, 5, 3, 2))
	assert(vector(10, 9, 8, 7) // vector(1, 2, 3, 4) == vector(10, 4, 2, 1))
else
	assert(10 // vector(1, 2, 3) == vector(10, 5, 3))
	assert(vector(10, 9, 8) // vector(1, 2, 3) == vector(10, 4, 2))
end

-- test NaN comparison
local nanv = vector(0/0, 0/0, 0/0)
assert(nanv ~= nanv);

-- __index
assert(vector(1, 2, 2).Magnitude == 3)
assert(vector(0, 0, 0)['Dot'](vector(1, 2, 4), vector(5, 6, 7)) == 45)
assert(vector(2, 0, 0).Unit == vector(1, 0, 0))

-- __namecall
assert(vector(1, 2, 4):Dot(vector(5, 6, 7)) == 45)
assert(ecall(function() vector(1, 2, 4):Dot() end) == "missing argument #2 (vector expected)")
assert(ecall(function() vector(1, 2, 4):Dot("a") end) == "invalid argument #2 (vector expected, got string)")

local function doDot1(a: vector, b)
	return a:Dot(b)
end

local function doDot2(a: vector, b)
	return (a:Dot(b))
end

local v124 = vector(1, 2, 4)

assert(doDot1(v124, vector(5, 6, 7)) == 45)
assert(doDot2(v124, vector(5, 6, 7)) == 45)
assert(ecall(function() doDot1(v124, "a") end) == "invalid argument #2 (vector expected, got string)")
assert(ecall(function() doDot2(v124, "a") end) == "invalid argument #2 (vector expected, got string)")
assert(select("#", doDot1(v124, vector(5, 6, 7))) == 1)
assert(select("#", doDot2(v124, vector(5, 6, 7))) == 1)

-- can't use vector with NaN components as table key
assert(pcall(function() local t = {} t[vector(0/0, 2, 3)] = 1 end) == false)
assert(pcall(function() local t = {} t[vector(1, 0/0, 3)] = 1 end) == false)
assert(pcall(function() local t = {} t[vector(1, 2, 0/0)] = 1 end) == false)
assert(pcall(function() local t = {} rawset(t, vector(0/0, 2, 3), 1) end) == false)

assert(vector(1, 0, 0):Cross(vector(0, 1, 0)) == vector(0, 0, 1))
assert(vector(0, 1, 0):Cross(vector(1, 0, 0)) == vector(0, 0, -1))

-- make sure we cover both builtin and C impl
assert(vector(1, 2, 4) == vector("1", "2", "4"))

-- validate component access (both cases)
assert(vector(1, 2, 3).x == 1)
assert(vector(1, 2, 3).X == 1)
assert(vector(1, 2, 3).y == 2)
assert(vector(1, 2, 3).Y == 2)
assert(vector(1, 2, 3).z == 3)
assert(vector(1, 2, 3).Z == 3)

-- additional checks for 4-component vectors
if vector_size == 4 then
	assert(vector(1, 2, 3, 4).w == 4)
	assert(vector(1, 2, 3, 4).W == 4)
end

-- negative zero should hash the same as zero
-- note: our earlier test only really checks the low hash bit, so in absence of perfect avalanche it's insufficient
do
	local larget = {}
	for i = 1, 2^14 do
	    larget[vector(0, 0, i)] = true
	end

	larget[vector(0, 0, 0)] = 42

	assert(larget[vector(0, 0, 0)] == 42)
	assert(larget[vector(0, 0, -0)] == 42)
	assert(larget[vector(0, -0, 0)] == 42)
	assert(larget[vector(-0, 0, 0)] == 42)
end

local function numvectemporary()
  local proptab = {}

  proptab.vec3compsum = function(vec: vector)
    local num = vec.X + vec.Y
    local tmp = vec / num
    local num2 = num * 2
    return tmp, num2
  end

  local a, b = proptab.vec3compsum(vector(2, 6, 0))

  assert(a.X == 0.25)
  assert(a.Y == 0.75)
  assert(b == 16)
end

numvectemporary()

return 'OK'
