-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing vectors')

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
assert(tostring(vector(1, 2, 3)) == "1, 2, 3")
assert(tostring(vector(-1, 2, 0.5)) == "-1, 2, 0.5")

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

assert(vector(1, 2, 4) / vector(8, 16, 24) == vector(1/8, 2/16, 4/24));
assert(vector(1, 2, 4) / 8 == vector(1/8, 1/4, 1/2));
assert(vector(1, 2, 4) / (1 / val) == vector(1/8, 2/8, 4/8));
assert(8 / vector(8, 16, 24) == vector(1, 1/2, 1/3));
assert(vector(1, 2, 4) / '8' == vector(1/8, 1/4, 1/2));
assert('8' / vector(8, 16, 24) == vector(1, 1/2, 1/3));

assert(-vector(1, 2, 4) == vector(-1, -2, -4));

-- test NaN comparison
local nanv = vector(0/0, 0/0, 0/0)
assert(nanv ~= nanv);

-- __index
assert(vector(1, 2, 2).Magnitude == 3)
assert(vector(0, 0, 0)['Dot'](vector(1, 2, 4), vector(5, 6, 7)) == 45)

-- __namecall
assert(vector(1, 2, 4):Dot(vector(5, 6, 7)) == 45)

-- can't use vector with NaN components as table key
assert(pcall(function() local t = {} t[vector(0/0, 2, 3)] = 1 end) == false)
assert(pcall(function() local t = {} t[vector(1, 0/0, 3)] = 1 end) == false)
assert(pcall(function() local t = {} t[vector(1, 2, 0/0)] = 1 end) == false)
assert(pcall(function() local t = {} rawset(t, vector(0/0, 2, 3), 1) end) == false)

-- make sure we cover both builtin and C impl
assert(vector(1, 2, 4) == vector("1", "2", "4"))

return 'OK'
