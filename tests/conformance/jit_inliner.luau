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

do
    -- check that feedback slots are correctly remapped
    function recursive(n)
        if n > 0 then
            return (recursive(n - 1))
        end
        return 0
    end

    function caller(n)
        return (recursive(n))
    end

    for _ = 1, INLINING_THREASHOLD do
        assert(caller(0) == 0)
    end

    assert(caller(1) == 0)
end

class InlinedPoint
    public x
    public y
end

do
    -- check that class construction shape feedback slots are adjusted correctly
    local function makePoint(Class, x)
        local result = Class { x = x, y = 42 }
        return result
    end

    local function caller(x)
        local marker = "caller constant"
        local point = makePoint(InlinedPoint, x)
        return marker, point
    end

    for i = 1, INLINING_THREASHOLD do
        local marker, point = caller(i)
        assert(marker == "caller constant")
        assert(point.x == i)
        assert(point.y == 42)
    end
end

function fuzzfail_infinite1()
    local function foo(x)
        repeat
            x = nil
            (function(...) end)()
        until x

        -- unreachable SCC that closes upvalues
        repeat
            local y = {}
        until not function() y = nil end
    end

    for _ in foo do end
end

function fuzzfail_infinite2()
    local f = function(...) end
    local x = 1
    while true do
        f(1, function(...)
            x *= x
        end)
    end
end

function fuzzfail_infinite3()
    function _()
        (function(a)
            for k in _() do
            end
        end)()
        while 0 do
        end
    end
    _()
end

-- repeated inlining of a caller whose feedback vector held a stale call_target.pc, either from a sealed site or a block removed by constant folding,
-- tripped an assert reading insnsPC[stale_pc] in buildGraphFromProto
local stalePcCount = 0
function fuzzfail_stale_feedback_pc()
    do end
    repeat
        stalePcCount += 1
        function _(l0:boolean,...)
            function _():Part
                return `{_}`
            end
            repeat
                break
            until _(_ { },`{_({_=_,})}{nil}{_ { }}`)
            _ = _ { },_ { }
        end
    until stalePcCount > 35 or _()
end

-- test that native function counters refer to the correct Proto after promotion
function counters_proto_promotion()
    local function add(a, b) return a + b end

    local P
    P = function(t: {[string]: any}, depth: number)
        local x = t.len -- entry guard exits to VM when t isn't a table
        local s = 0
        for i = 1, 100 do
            s = add(s, i) -- interpreted frames record feedback, triggering inlining
        end
        if depth == 2 then
            P("not a table", 1)
        elseif depth == 1 then
            P({}, 0) -- promotes the closure to the optimized proto
        end
        for i = 1, 3 do s += i end
        return s
    end

    assert(P({}, 2) == 5056)
end

return 'OK'
