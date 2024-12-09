-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing userdata')

function ecall(fn, ...)
	local ok, err = pcall(fn, ...)
	assert(not ok)
	return err:sub((err:find(": ") or -1) + 2, #err)
end

local function realmad(a: vec2, b: vec2, c: vec2): vec2
	return -c + a * b;
end

local function dm(s: vec2, t: vec2, u: vec2)
	local x = s:Dot(t)
	assert(x == 13)

	local t = u:Min(s)
	assert(t.X == 5)
	assert(t.Y == 4)
end

local s: vec2 = vec2(5, 4)
local t: vec2 = vec2(1, 2)
local u: vec2 = vec2(10, 20)

local x: vec2 = realmad(s, t, u)

assert(x.X == -5)
assert(x.Y == -12)

dm(s, t, u)

local function mu(v: vec2)
	assert(v.Magnitude == 2)
	assert(v.Unit.X == 0)
	assert(v.Unit.Y == 1)
end

mu(vec2(0, 2))

return 'OK'
