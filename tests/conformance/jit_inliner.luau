-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

const INLINING_THREASHOLD = 64

do
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

    for _ = 1, INLINING_THREASHOLD do
        assert(caller() == 56.0)
    end
end

do
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

    for i = 0, INLINING_THREASHOLD do
        local v = selectPath(2, i, 10, 3)
        local w = clamp(v, 0, 1000)
        assert(w == i * 3)
    end
end

do
    local function inlinee()
        local function inner() return 67 end
        return 42 + inner()
    end

    local function caller()
        inlinee()
    end

    for _ = 1, INLINING_THREASHOLD do
        caller()
    end
end

do
    -- select("#", ...) / select(i, ...) inside a vararg function (Immutable.Append pattern).
    -- select fuses to a FASTCALL with GETVARARGS between it and the paired CALL; inlining it would
    -- shift the CALL and break the FASTCALL skip offset. The inliner must refuse to inline `append`.
    local function append(list, ...)
        local new = {}
        local len = #list
        for key = 1, len do
            new[key] = list[key]
        end
        for i = 1, select("#", ...) do
            new[len + i] = select(i, ...)
        end
        return new
    end

    local function run()
        local r = append({ 1, 2 }, 7, 8, 9)
        return #r * 10000 + r[3] * 100 + r[5]
    end

    for _ = 1, INLINING_THREASHOLD do
        assert(run() == 50709, "vararg select must not be miscompiled by inlining")
    end
end

return 'OK'
