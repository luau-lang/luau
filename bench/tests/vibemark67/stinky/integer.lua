--!strict

-- Convenience wrappers around the global `integer` module so the rest of the
-- emulator can do 64-bit arithmetic without repeating boilerplate.

local i = integer

local M = {}

M.ZERO = i.create(0)
M.ONE = i.create(1)
M.NEG_ONE = i.neg(i.create(1))
M.MAX_U64 = M.NEG_ONE -- 0xFFFFFFFFFFFFFFFF as bits
M.MASK32 = i.fromstring("FFFFFFFF", 16)
M.MASK16 = i.create(0xFFFF)
M.MASK8 = i.create(0xFF)

function M.from(n: number): integer
    return i.create(n)
end

function M.fromHex(s: string): integer
    return i.fromstring(s, 16)
end

function M.toNumber(x: integer): number
    return i.tonumber(x)
end

function M.add(a: integer, b: integer): integer
    return i.add(a, b)
end

function M.sub(a: integer, b: integer): integer
    return i.sub(a, b)
end

function M.mul(a: integer, b: integer): integer
    return i.mul(a, b)
end

function M.neg(a: integer): integer
    return i.neg(a)
end

function M.band(a: integer, b: integer): integer
    return i.band(a, b)
end

function M.bor(a: integer, b: integer): integer
    return i.bor(a, b)
end

function M.bxor(a: integer, b: integer): integer
    return i.bxor(a, b)
end

function M.bnot(a: integer): integer
    return i.bnot(a)
end

function M.lshift(a: integer, n: integer): integer
    return i.lshift(a, n)
end

function M.rshift(a: integer, n: integer): integer
    return i.rshift(a, n)
end

function M.arshift(a: integer, n: integer): integer
    return i.arshift(a, n)
end

-- Shift by a number (converts to integer internally)
function M.shl(a: integer, n: number): integer
    return i.lshift(a, i.create(n))
end

function M.shr(a: integer, n: number): integer
    return i.rshift(a, i.create(n))
end

function M.sar(a: integer, n: number): integer
    return i.arshift(a, i.create(n))
end

function M.eq(a: integer, b: integer): boolean
    return a == b
end

function M.lt(a: integer, b: integer): boolean
    return i.lt(a, b)
end

function M.le(a: integer, b: integer): boolean
    return i.le(a, b)
end

function M.gt(a: integer, b: integer): boolean
    return i.gt(a, b)
end

function M.ge(a: integer, b: integer): boolean
    return i.ge(a, b)
end

function M.ult(a: integer, b: integer): boolean
    return i.ult(a, b)
end

function M.ule(a: integer, b: integer): boolean
    return i.ule(a, b)
end

function M.ugt(a: integer, b: integer): boolean
    return i.ugt(a, b)
end

function M.uge(a: integer, b: integer): boolean
    return i.uge(a, b)
end

function M.udiv(a: integer, b: integer): integer
    return i.udiv(a, b)
end

function M.sdiv(a: integer, b: integer): integer
    return i.div(a, b)
end

function M.urem(a: integer, b: integer): integer
    return i.urem(a, b)
end

function M.srem(a: integer, b: integer): integer
    return i.rem(a, b)
end

-- Sign-extend a value from `bits` width to 64 bits.
function M.signExtend(val: integer, bits: number): integer
    local shift = 64 - bits
    local shiftI = i.create(shift)
    return i.arshift(i.lshift(val, shiftI), shiftI)
end

-- Zero-extend (mask to `bits` width).
function M.zeroExtend(val: integer, bits: number): integer
    if bits >= 64 then return val end
    local mask = i.sub(i.lshift(M.ONE, i.create(bits)), M.ONE)
    return i.band(val, mask)
end

-- Extract bits [hi:lo] inclusive from val.
function M.extractBits(val: integer, lo: number, hi: number): integer
    local width = hi - lo + 1
    local shifted = M.shr(val, lo)
    return M.zeroExtend(shifted, width)
end

-- Count leading zeros (64-bit).
function M.clz64(val: integer): number
    return i.tonumber(i.countlz(val))
end

-- Reverse bits of a 64-bit value.
function M.rbit64(val: integer): integer
    local result = M.ZERO
    for bit = 0, 63 do
        if i.btest(val, i.lshift(M.ONE, i.create(bit))) then
            result = i.bor(result, i.lshift(M.ONE, i.create(63 - bit)))
        end
    end
    return result
end

-- Reverse bytes of a 64-bit value.
function M.rev64(val: integer): integer
    local result = M.ZERO
    for byte = 0, 7 do
        local b = i.band(i.rshift(val, i.create(byte * 8)), M.MASK8)
        result = i.bor(result, i.lshift(b, i.create((7 - byte) * 8)))
    end
    return result
end

-- Reverse bytes of lower 32 bits.
function M.rev32(val: integer): integer
    local result = M.ZERO
    for byte = 0, 3 do
        local b = i.band(i.rshift(val, i.create(byte * 8)), M.MASK8)
        result = i.bor(result, i.lshift(b, i.create((3 - byte) * 8)))
    end
    return result
end

-- Reverse bytes in each 16-bit halfword of lower 32 bits.
function M.rev16(val: integer): integer
    local b0 = i.band(val, M.MASK8)
    local b1 = i.band(i.rshift(val, i.create(8)), M.MASK8)
    local b2 = i.band(i.rshift(val, i.create(16)), M.MASK8)
    local b3 = i.band(i.rshift(val, i.create(24)), M.MASK8)
    return i.bor(i.bor(i.lshift(b0, i.create(8)), b1),
                 i.bor(i.lshift(b2, i.create(24)), i.lshift(b3, i.create(16))))
end

function M.isZero(val: integer): boolean
    return val == M.ZERO
end

function M.isNegative(val: integer): boolean
    return i.lt(val, M.ZERO)
end

-- Multiply two 64-bit values and return the high 64 bits (signed).
function M.smulh(a: integer, b: integer): integer
    -- Use the integer module's mul which gives low 64 bits.
    -- For smulh we need to do it differently. Let's split into 32-bit halves.
    local a_neg = M.isNegative(a)
    local b_neg = M.isNegative(b)
    local abs_a = if a_neg then M.neg(a) else a
    local abs_b = if b_neg then M.neg(b) else b
    local hi = M.umulh_impl(abs_a, abs_b)
    if a_neg ~= b_neg then
        -- negate 128-bit result: complement high, and if low != 0, subtract 1 from high
        local lo = i.mul(abs_a, abs_b)
        hi = i.bnot(hi)
        if lo ~= M.ZERO then
            hi = i.add(hi, M.ONE)
        end
    end
    return hi
end

-- Multiply two 64-bit values and return the high 64 bits (unsigned).
function M.umulh(a: integer, b: integer): integer
    return M.umulh_impl(a, b)
end

function M.umulh_impl(a: integer, b: integer): integer
    -- Split each into two 32-bit halves and do schoolbook multiplication.
    local a_lo = i.band(a, M.MASK32)
    local a_hi = i.rshift(a, i.create(32))
    local b_lo = i.band(b, M.MASK32)
    local b_hi = i.rshift(b, i.create(32))

    -- a*b = (a_hi*2^32 + a_lo) * (b_hi*2^32 + b_lo)
    --     = a_hi*b_hi*2^64 + (a_hi*b_lo + a_lo*b_hi)*2^32 + a_lo*b_lo
    -- We want bits [127:64]

    -- Since these are positive and fit in 63 bits each half fits in 32 bits unsigned.
    -- But the integer module treats them as signed 64-bit...
    -- The products of 32-bit * 32-bit fit in 64 bits unsigned.
    -- However integer.mul gives us the low 64 bits which IS the correct product for 32x32.

    local ll = i.mul(a_lo, b_lo)
    local lh = i.mul(a_lo, b_hi)
    local hl = i.mul(a_hi, b_lo)
    local hh = i.mul(a_hi, b_hi)

    -- ll contributes bits [63:0], so ll >> 32 carries into the middle sum
    local ll_hi = i.rshift(ll, i.create(32))

    -- middle = lh + hl + ll_hi (but this can overflow 64 bits by at most 1 bit)
    local mid = i.add(lh, ll_hi)
    -- detect carry: if mid < lh (unsigned), carry occurred
    local carry1: integer = if i.ult(mid, lh) then M.ONE else M.ZERO
    local mid2 = i.add(mid, hl)
    local carry2: integer = if i.ult(mid2, hl) then M.ONE else M.ZERO

    -- high = hh + (carry1 + carry2) * 2^32 + mid2 >> 32
    local mid2_hi = i.rshift(mid2, i.create(32))
    local carries = i.add(carry1, carry2)
    local result = i.add(hh, mid2_hi)
    result = i.add(result, i.lshift(carries, i.create(32)))

    return result
end

return M
