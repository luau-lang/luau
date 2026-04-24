-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print('testing userdata direct access')

local function fuzzyeq(a, b)
    return math.abs(a - b) < 0.001
end

-- direct vec2 property reads
local v = vec2(3, 4)
assert(v.X == 3)
assert(v.Y == 4)
assert(v.Magnitude == 5)

local u = v.Unit
assert(fuzzyeq(u.X, 0.6))
assert(fuzzyeq(u.Y, 0.8))

-- repeated reads exercise the cached slot fast path
for i = 1, 20 do
    assert(v.X == 3)
    assert(v.Y == 4)
end

-- direct vertex property reads
local vtx = vertex(vector.create(1, 2, 3), vector.create(0.5, 0, 0.5), vec2(0.25, 0.5))
assert(vtx.pos.X == 1 and vtx.pos.Y == 2 and vtx.pos.Z == 3)
assert(fuzzyeq(vtx.normal.X, 0.5) and vtx.normal.Y == 0 and fuzzyeq(vtx.normal.Z, 0.5))
assert(vtx.uv.X == 0.25 and vtx.uv.Y == 0.5)

-- direct vec2 property writes
v.X = 10
v.Y = 20
assert(v.X == 10)
assert(v.Y == 20)

-- repeated writes exercise cached slot
for i = 1, 20 do
    v.X = i
    v.Y = i * 2
    assert(v.X == i)
    assert(v.Y == i * 2)
end

-- direct vertex property writes
vtx.uv = vec2(0.75, 0.25)
assert(vtx.uv.X == 0.75 and vtx.uv.Y == 0.25)

vtx.pos = vector.create(10, 20, 30)
assert(vtx.pos.X == 10 and vtx.pos.Y == 20 and vtx.pos.Z == 30)

vtx.normal = vector.create(0, 1, 0)
assert(vtx.normal.Y == 1)

-- direct vec2 method calls
local a = vec2(1, 2)
local b = vec2(3, 4)
assert(a:Dot(b) == 11)

local m = a:Min(b)
assert(m.X == 1 and m.Y == 2)

local c = a:Clone()
assert(c.X == 1 and c.Y == 2)

-- repeated calls exercise cached slot
for i = 1, 20 do
    assert(a:Dot(b) == 11)
end

-- direct vertex method calls
local vtxClone = vtx:Clone()
assert(vtxClone.pos.X == vtx.pos.X and vtxClone.pos.Y == vtx.pos.Y and vtxClone.pos.Z == vtx.pos.Z)
assert(vtxClone.uv.X == vtx.uv.X and vtxClone.uv.Y == vtx.uv.Y)

-- polymorphic property reads (shared cached slot)
assert(v.sizeof == 8)
assert(vtx.sizeof == 32)

local function getSizeof(obj)
    return obj.sizeof
end

assert(getSizeof(v) == 8)
assert(getSizeof(vtx) == 32)
assert(getSizeof(v) == 8)

for i = 1, 20 do
    assert(getSizeof(v) == 8)
    assert(getSizeof(vtx) == 32)
end

-- polymorphic method calls (shared cached slot)
local function callClone(obj)
    return obj:Clone()
end

local cv = callClone(v)
assert(cv.X == v.X and cv.Y == v.Y)

local cvtx = callClone(vtx)
assert(cvtx.pos.X == vtx.pos.X)

for i = 1, 20 do
    local r1 = callClone(v)
    assert(r1.X == v.X)
    local r2 = callClone(vtx)
    assert(r2.pos.X == vtx.pos.X)
end

-- fallback to generic property read
-- when a non-userdata value is encountered, direct access falls back to the generic path
local function readField(obj)
    return obj.X
end

local t = {X = 42}
assert(readField(t) == 42)
assert(readField(vec2(7, 8)) == 7)
assert(readField({X = 99}) == 99)

-- fallback to generic property write
local function writeField(obj, val)
    obj.X = val
end

local wt = {}
writeField(wt, 42)
assert(wt.X == 42)

local wv = vec2(0, 0)
writeField(wv, 99)
assert(wv.X == 99)

-- fallback to generic method call
local function callDot(a, b)
    return a:Dot(b)
end

local fakevec = {Dot = function(self, other) return 999 end}
assert(callDot(fakevec, fakevec) == 999)
assert(callDot(vec2(1, 2), vec2(3, 4)) == 11)

-- chained access across types
local vtx3 = vertex(vector.create(5, 6, 7), vector.create(0, 0, 1), vec2(0.125, 0.875))
assert(vtx3.uv.X == 0.125)
assert(vtx3.uv.Y == 0.875)

local mag = vtx3.uv.Magnitude
assert(fuzzyeq(mag, math.sqrt(0.125 * 0.125 + 0.875 * 0.875)))

local dotResult = vtx3.uv:Dot(vec2(1, 0))
assert(dotResult == 0.125)

-- check interactions on VM/NCG switch
function guardedReadX(obj, _guard: number)
    return obj.X
end

function guardedWriteX(obj, val, _guard: number)
    obj.X = val
end

function guardedCallDot(a, b, _guard: number)
    return a:Dot(b)
end

do
    guardedReadX(vec2(3, 4), "vm")
    guardedReadX(vec2(3, 4), "vm")
    assert(guardedReadX(vec2(7, 8), 0) == 7)
end

do
    local w = vec2(0, 0)
    guardedWriteX(w, 42, "vm")
    guardedWriteX(w, 99, "vm")

    local v = vec2(10, 20)
    guardedWriteX(v, 55, 0)
    assert(v.X == 55)
end

do
    guardedCallDot(vec2(1, 2), vec2(3, 4), "vm")
    guardedCallDot(vec2(1, 2), vec2(3, 4), "vm")
    assert(guardedCallDot(vec2(1, 2), vec2(3, 4), 0) == 11)
end

return 'OK'
