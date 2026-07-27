-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

local function clamp(x, lo, hi)
    if x < lo then return lo
    elseif x > hi then return hi
    else return x end
end

local function selectPath(mode, a, b, c)
    if mode == 1 then return a + b
    elseif mode == 2 then return a * c
    else return 0 end
end

local function compute(a, b)
    local c = a + b
    local d = c * 4.0
    local e = d - 0.5
    local f = e / 2.0
    local g = f * 4.0
    local h = g + 1.0
    local i = h - 0.5
    local j = i + 8.5
    local k = j * 2.0
    return k
end

local function caller()
    return compute(2.0, 0.5)
end

for _ = 1, 40 do
    assert(caller() == 56.0)
end

for i = 0, 40 do
    local v = selectPath(2, i, 10, 3)
    local w = clamp(v, 0, 1000)
    assert(w == i * 3)
end

return 'OK'
