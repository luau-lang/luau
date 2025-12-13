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

local v1: vertex = vertex(vector.create(1, 2, 3), vector.create(0.5, 0, 0.5), vec2(0.25, 0.5))

assert(v1.pos.X == 1 and v1.pos.Y == 2 and v1.pos.Z == 3)
assert(v1.normal.x == 0.5 and v1.normal.Y == 0 and v1.normal.Z == 0.5)
assert(v1.uv.X == 0.25)
assert(v1.uv.Y == 0.5)

local function vertextest1(v: vertex)
	return v.uv.X * v1.pos:Dot(v1.normal)
end

assert(vertextest1(v1) == 0.5)

return 'OK'
