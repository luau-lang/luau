-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing native integer register spills")

local function test_many_locals(
    p0: integer, p1: integer, p2: integer, p3: integer
): integer
    assert(is_native())

    -- create many live values from various operations
    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.bxor(p2, p3)
    local d: integer = integer.band(p0, p3)
    local e: integer = integer.bor(p1, p2)
    local f: integer = integer.add(p2, p3)
    local g: integer = integer.bxor(a, b)
    local h: integer = integer.band(c, d)
    local i: integer = integer.bor(e, f)
    local j: integer = integer.add(a, c)
    local k: integer = integer.sub(b, d)
    local l: integer = integer.bxor(e, g)
    local m: integer = integer.band(f, h)
    local n: integer = integer.bor(g, i)
    local o: integer = integer.add(h, j)
    local p: integer = integer.sub(i, k)
    local q: integer = integer.bxor(j, l)
    local r: integer = integer.band(k, m)
    local s: integer = integer.bor(l, n)
    local t: integer = integer.add(m, o)

    -- invalidates restore locations for old values.
    a = integer.add(a, t)
    b = integer.bxor(b, s)
    c = integer.band(c, r)
    d = integer.bor(d, q)
    e = integer.sub(e, p)
    f = integer.add(f, o)
    g = integer.bxor(g, n)
    h = integer.band(h, m)
    i = integer.bor(i, l)
    j = integer.add(j, k)

    k = integer.bxor(a, integer.add(b, c))
    l = integer.band(d, integer.sub(e, f))
    m = integer.bor(g, integer.bxor(h, i))
    n = integer.add(j, integer.band(k, l))
    o = integer.sub(m, integer.bor(n, a))

    return integer.bxor(
        integer.bxor(integer.bxor(a, b), integer.bxor(c, d)),
        integer.bxor(integer.bxor(e, f), integer.bxor(g, h)),
        integer.bxor(integer.bxor(i, j), integer.bxor(k, l)),
        integer.bxor(integer.bxor(m, n), integer.bxor(o, p)),
        integer.bxor(integer.bxor(q, r), integer.bxor(s, t))
    )
end

local function test_division_pressure(
    p0: integer, p1: integer, p2: integer, p3: integer
): integer
    assert(is_native())

    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.bxor(p2, p3)
    local d: integer = integer.band(p0, p3)
    local e: integer = integer.bor(p1, p2)
    local f: integer = integer.add(p2, p3)
    local g: integer = integer.bxor(a, b)
    local h: integer = integer.add(c, d)
    local i: integer = integer.bor(e, f)
    local j: integer = integer.bxor(g, h)
    local k: integer = integer.add(i, a)
    local l: integer = integer.bor(b, c)

    -- each div clobbers RAX+RDX, requiring spills of other live INT64 values
    local d1: integer = integer.div(a, p1)
    local d2: integer = integer.div(b, p2)
    local d3: integer = integer.rem(c, p3)
    local d4: integer = integer.mod(f, p0)
    local d5: integer = integer.udiv(integer.bor(e, 1i), p1)

    return integer.bxor(
        integer.bxor(integer.bxor(a, b), integer.bxor(c, d)),
        integer.bxor(integer.bxor(e, f), integer.bxor(g, h)),
        integer.bxor(integer.bxor(i, j), integer.bxor(k, l)),
        integer.bxor(integer.bxor(d1, d2), integer.bxor(d3, d4)),
        d5
    )
end

-- shifts require RCX
local function test_shift_pressure(
    p0: integer, p1: integer, p2: integer, p3: integer
): integer
    assert(is_native())

    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.bxor(p2, p3)
    local d: integer = integer.band(p0, p3)
    local e: integer = integer.bor(p1, p2)
    local f: integer = integer.add(p2, p3)
    local g: integer = integer.bxor(a, b)
    local h: integer = integer.add(c, d)
    local i: integer = integer.bor(e, f)
    local j: integer = integer.bxor(g, h)
    local k: integer = integer.add(i, a)
    local l: integer = integer.bor(b, c)

    local sh1: integer = integer.band(p0, 7i)  -- 0..7
    local sh2: integer = integer.band(p1, 15i) -- 0..15
    local sh3: integer = integer.band(p2, 31i) -- 0..31
    local sh4: integer = integer.band(p3, 63i) -- 0..63

    local s1: integer = integer.lshift(a, sh1)
    local s2: integer = integer.rshift(b, sh2)
    local s3: integer = integer.arshift(c, sh3)
    local s4: integer = integer.lrotate(d, sh4)
    local s5: integer = integer.rrotate(e, sh1)
    local s6: integer = integer.lshift(f, sh2)
    local s7: integer = integer.rshift(g, sh3)
    local s8: integer = integer.arshift(h, sh4)

    return integer.bxor(
        integer.bxor(integer.bxor(a, b), integer.bxor(c, d)),
        integer.bxor(integer.bxor(e, f), integer.bxor(g, h)),
        integer.bxor(integer.bxor(i, j), integer.bxor(k, l)),
        integer.bxor(integer.bxor(s1, s2), integer.bxor(s3, s4)),
        integer.bxor(integer.bxor(s5, s6), integer.bxor(s7, s8))
    )
end

-- regalloc may need to spill registers while in the loop
local function test_loop_spill(
    p0: integer, p1: integer, p2: integer, p3: integer,
    iters: number
): integer
    assert(is_native())

    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.bxor(p2, p3)
    local d: integer = integer.band(p0, p3)
    local e: integer = integer.bor(p1, p2)
    local f: integer = integer.add(p2, p3)
    local g: integer = integer.bxor(a, b)
    local h: integer = integer.add(c, d)
    local i: integer = integer.bor(e, f)
    local j: integer = integer.bxor(g, h)
    local k: integer = integer.add(i, a)
    local l: integer = integer.bor(b, c)

    for iter = 1, iters do
        -- create register pressure within the loop
        a = integer.add(a, integer.bxor(b, c))
        b = integer.sub(b, integer.band(d, e))
        c = integer.bxor(c, integer.bor(f, g))
        d = integer.add(d, integer.bxor(h, i))
        e = integer.sub(e, integer.band(j, k))
        f = integer.bor(f, integer.add(l, a))
        g = integer.band(g, integer.sub(b, c))
        h = integer.bxor(h, integer.add(d, e))
        i = integer.bor(i, integer.sub(f, g))
        j = integer.add(j, integer.bxor(h, i))
        k = integer.sub(k, integer.band(j, a))
        l = integer.bxor(l, integer.bor(k, b))
    end

    return integer.bxor(
        integer.bxor(integer.bxor(a, b), integer.bxor(c, d)),
        integer.bxor(integer.bxor(e, f), integer.bxor(g, h)),
        integer.bxor(integer.bxor(i, j), integer.bxor(k, l))
    )
end

local function test_mixed_pressure(
    p0: integer, p1: integer, p2: integer, p3: integer
): integer
    assert(is_native())

    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.bxor(p2, p3)
    local d: integer = integer.band(p0, p3)
    local e: integer = integer.bor(p1, p2)
    local f: integer = integer.add(p2, p3)
    local g: integer = integer.bxor(a, b)
    local h: integer = integer.add(c, d)
    local i: integer = integer.bor(e, f)
    local j: integer = integer.bxor(g, h)
    local k: integer = integer.add(i, a)
    local l: integer = integer.bor(b, c)

    local m: integer = integer.bxor(a, b, c, d, e)
    local n: integer = integer.band(f, g, h, i, j)
    local o: integer = integer.bor(k, l, a, b, c)

    local d1: integer = integer.div(integer.add(a, 100i), p1)
    local d2: integer = integer.idiv(integer.add(b, 200i), p2)

    local sh: integer = integer.band(p0, 7i)
    local s1: integer = integer.lshift(m, sh)
    local s2: integer = integer.rshift(n, sh)

    local x: integer = integer.add(d1, s1)
    local y: integer = integer.bxor(d2, s2)
    local z: integer = integer.band(x, y)

    return integer.bxor(
        integer.bxor(integer.bxor(a, b), integer.bxor(c, d)),
        integer.bxor(integer.bxor(e, f), integer.bxor(g, h)),
        integer.bxor(integer.bxor(i, j), integer.bxor(k, l)),
        integer.bxor(integer.bxor(m, n), integer.bxor(o, d1)),
        integer.bxor(integer.bxor(d2, s1), integer.bxor(s2, z))
    )
end

-- 30 live locals must require register spilling
local function test_extreme_pressure(
    p0: integer, p1: integer, p2: integer, p3: integer
): integer
    assert(is_native())

    local a: integer = integer.add(p0, p1)
    local b: integer = integer.sub(p0, p1)
    local c: integer = integer.mul(p2, 3i)
    local d: integer = integer.bxor(p0, p3)
    local e: integer = integer.band(p1, p2)
    local f: integer = integer.bor(p2, p3)
    local g: integer = integer.bnot(p0)
    local h: integer = integer.lshift(p1, 3i)
    local i: integer = integer.rshift(p2, 2i)
    local j: integer = integer.arshift(p3, 1i)
    local k: integer = integer.lrotate(p0, 7i)
    local l: integer = integer.rrotate(p1, 5i)
    local m: integer = integer.add(a, b)
    local n: integer = integer.bxor(c, d)
    local o: integer = integer.band(e, f)

    local p: integer = integer.bor(g, h)
    local q: integer = integer.add(i, j)
    local r: integer = integer.sub(k, l)
    local s: integer = integer.bxor(m, n)
    local t: integer = integer.band(o, p)
    local u: integer = integer.bor(q, r)
    local v: integer = integer.add(s, t)
    local w: integer = integer.sub(u, a)
    local x: integer = integer.bxor(v, b)
    local y: integer = integer.band(w, c)
    local z: integer = integer.bor(x, d)
    local a2: integer = integer.add(y, e)
    local b2: integer = integer.sub(z, f)
    local c2: integer = integer.bxor(a2, g)
    local d2: integer = integer.band(b2, h)

    local div1: integer = integer.div(integer.bor(a, 100i), p1)
    local div2: integer = integer.div(integer.bor(m, 200i), p2)

    local sh: integer = integer.band(p0, 3i)
    local sh1: integer = integer.lshift(n, sh)
    local sh2: integer = integer.rshift(o, sh)

    local t1: integer = integer.bxor(integer.bxor(a, b), integer.bxor(c, d))
    local t2: integer = integer.bxor(integer.bxor(e, f), integer.bxor(g, h))
    local t3: integer = integer.bxor(integer.bxor(i, j), integer.bxor(k, l))
    local t4: integer = integer.bxor(integer.bxor(m, n), integer.bxor(o, p))
    local t5: integer = integer.bxor(integer.bxor(q, r), integer.bxor(s, t))
    local t6: integer = integer.bxor(integer.bxor(u, v), integer.bxor(w, x))
    local t7: integer = integer.bxor(integer.bxor(y, z), integer.bxor(a2, b2))
    local t8: integer = integer.bxor(integer.bxor(c2, d2), integer.bxor(div1, div2))
    local t9: integer = integer.bxor(sh1, sh2)
    return integer.bxor(
        integer.bxor(t1, t2, t3, t4, t5),
        integer.bxor(t6, t7, t8, t9)
    )
end

-- values with significant upper 32 bits to detect truncation for incorrect spill widths
local V0: integer = 0x1234567890ABCDEFi
local V1: integer = 0xFEDCBA0987654321i
local V2: integer = 0x0011223344556677i
local V3: integer = 0x8899AABBCCDDEEFFi
local V4: integer = 0xA5A5A5A5_5A5A5A5Ai

local r1 = test_many_locals(V0, V1, V2, V3)
local r2 = test_division_pressure(V0, V1, V2, V3)
local r3 = test_shift_pressure(V0, V1, V2, V3)
local r4 = test_loop_spill(V0, V1, V2, V3, 2)
local r5 = test_mixed_pressure(V0, V1, V2, V3)
local r6 = test_extreme_pressure(V0, V1, V2, V3)

assert(type(r1) == "integer")
assert(type(r2) == "integer")
assert(type(r3) == "integer")
assert(type(r4) == "integer")
assert(type(r5) == "integer")
assert(type(r6) == "integer")

assert(test_many_locals(V0, V1, V2, V3) == r1)
assert(test_division_pressure(V0, V1, V2, V3) == r2)
assert(test_shift_pressure(V0, V1, V2, V3) == r3)
assert(test_loop_spill(V0, V1, V2, V3, 2) == r4)

return "OK"
