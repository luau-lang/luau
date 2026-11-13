--!strict

-- Immutable register bit patterns, stored as unsigned low/high 32-bit words.
-- Only conversion to/from numbers can lose precision above 2^53.
export type Int64 = { lo: number, hi: number }

local WORD = 0x100000000
local HALF = 0x10000
local SIGN = 0x80000000
local ZERO: Int64 = { lo = 0, hi = 0 }
local ONE: Int64 = { lo = 1, hi = 0 }
local NEG_ONE: Int64 = { lo = WORD - 1, hi = WORD - 1 }
local M = {}

-- Inputs are unsigned 32-bit words; callers must not mutate the returned value.
function M.fromWords(lo: number, hi: number): Int64
    if lo == 0 and hi == 0 then return ZERO end
    return { lo = lo, hi = hi }
end

M.ZERO = ZERO
M.ONE = ONE
M.NEG_ONE = NEG_ONE
M.MAX_U64 = NEG_ONE
M.MASK32 = M.fromWords(WORD - 1, 0)
M.MASK16 = M.fromWords(HALF - 1, 0)
M.MASK8 = M.fromWords(0xFF, 0)

function M.from(n: number): Int64
    assert(n == math.floor(n) and n > -math.huge and n < math.huge, "Expected an integral number")
    return M.fromWords(n % WORD, math.floor(n / WORD) % WORD)
end

function M.fromHex(s: string): Int64
    assert(#s > 0 and #s <= 16 and string.match(s, "^[%da-fA-F]+$"), "Expected a 64-bit hex bit pattern")
    local split = math.max(0, #s - 8)
    local hi = if split == 0 then 0 else (tonumber(string.sub(s, 1, split), 16) :: number)
    local lo = tonumber(string.sub(s, split + 1), 16) :: number
    return M.fromWords(lo, hi)
end

function M.toNumber(x: Int64): number
    local hi = if x.hi >= SIGN then x.hi - WORD else x.hi
    return hi * WORD + x.lo
end

function M.add(a: Int64, b: Int64): Int64
    local lo = a.lo + b.lo
    return M.fromWords(lo % WORD, (a.hi + b.hi + math.floor(lo / WORD)) % WORD)
end

function M.sub(a: Int64, b: Int64): Int64
    local lo = a.lo - b.lo
    return M.fromWords(lo % WORD, (a.hi - b.hi - (if lo < 0 then 1 else 0)) % WORD)
end

function M.neg(a: Int64): Int64
    return M.sub(ZERO, a)
end

-- Schoolbook multiplication in base 2^16 keeps every intermediate exact in a double.
local function mul32(a: number, b: number): (number, number)
    local a0, a1 = a % HALF, math.floor(a / HALF)
    local b0, b1 = b % HALF, math.floor(b / HALF)
    local low = a0 * b0
    local middle = a1 * b0 + a0 * b1 + math.floor(low / HALF)
    return low % HALF + (middle % HALF) * HALF, a1 * b1 + math.floor(middle / HALF)
end

function M.mul(a: Int64, b: Int64): Int64
    local lo, hi = mul32(a.lo, b.lo)
    local cross1 = mul32(a.lo, b.hi)
    local cross2 = mul32(a.hi, b.lo)
    return M.fromWords(lo, (hi + cross1 + cross2) % WORD)
end

function M.umulh(a: Int64, b: Int64): Int64
    local _, llHi = mul32(a.lo, b.lo)
    local lhLo, lhHi = mul32(a.lo, b.hi)
    local hlLo, hlHi = mul32(a.hi, b.lo)
    local hhLo, hhHi = mul32(a.hi, b.hi)
    local middle = llHi + lhLo + hlLo
    local high = hhLo + lhHi + hlHi + math.floor(middle / WORD)
    return M.fromWords(high % WORD, (hhHi + math.floor(high / WORD)) % WORD)
end

function M.smulh(a: Int64, b: Int64): Int64
    local high = M.umulh(a, b)
    if a.hi >= SIGN then high = M.sub(high, b) end
    if b.hi >= SIGN then high = M.sub(high, a) end
    return high
end

function M.band(a: Int64, b: Int64): Int64
    return M.fromWords(bit32.band(a.lo, b.lo), bit32.band(a.hi, b.hi))
end

function M.bor(a: Int64, b: Int64): Int64
    return M.fromWords(bit32.bor(a.lo, b.lo), bit32.bor(a.hi, b.hi))
end

function M.bxor(a: Int64, b: Int64): Int64
    return M.fromWords(bit32.bxor(a.lo, b.lo), bit32.bxor(a.hi, b.hi))
end

function M.bnot(a: Int64): Int64
    return M.fromWords(bit32.bnot(a.lo), bit32.bnot(a.hi))
end

function M.shl(a: Int64, n: number): Int64
    if n < 0 then return M.shr(a, -n) end
    if n >= 64 then return ZERO end
    if n == 0 then return a end
    if n >= 32 then return M.fromWords(0, bit32.lshift(a.lo, n - 32)) end
    return M.fromWords(bit32.lshift(a.lo, n), bit32.bor(bit32.lshift(a.hi, n), bit32.rshift(a.lo, 32 - n)))
end

function M.shr(a: Int64, n: number): Int64
    if n < 0 then return M.shl(a, -n) end
    if n >= 64 then return ZERO end
    if n == 0 then return a end
    if n >= 32 then return M.fromWords(bit32.rshift(a.hi, n - 32), 0) end
    return M.fromWords(bit32.bor(bit32.rshift(a.lo, n), bit32.lshift(a.hi, 32 - n)), bit32.rshift(a.hi, n))
end

function M.sar(a: Int64, n: number): Int64
    if n < 0 then return M.shl(a, -n) end
    local sign = if a.hi >= SIGN then WORD - 1 else 0
    if n >= 64 then return M.fromWords(sign, sign) end
    if n == 0 then return a end
    if n >= 32 then return M.fromWords(bit32.arshift(a.hi, n - 32), sign) end
    return M.fromWords(bit32.bor(bit32.rshift(a.lo, n), bit32.lshift(a.hi, 32 - n)), bit32.arshift(a.hi, n))
end

function M.lshift(a: Int64, n: Int64): Int64
    return M.shl(a, M.toNumber(n))
end

function M.rshift(a: Int64, n: Int64): Int64
    return M.shr(a, M.toNumber(n))
end

function M.arshift(a: Int64, n: Int64): Int64
    return M.sar(a, M.toNumber(n))
end

function M.eq(a: Int64, b: Int64): boolean
    return a.hi == b.hi and a.lo == b.lo
end

function M.ult(a: Int64, b: Int64): boolean
    return a.hi < b.hi or (a.hi == b.hi and a.lo < b.lo)
end

function M.ule(a: Int64, b: Int64): boolean
    return not M.ult(b, a)
end

function M.ugt(a: Int64, b: Int64): boolean
    return M.ult(b, a)
end

function M.uge(a: Int64, b: Int64): boolean
    return not M.ult(a, b)
end

function M.lt(a: Int64, b: Int64): boolean
    if (a.hi >= SIGN) ~= (b.hi >= SIGN) then return a.hi >= SIGN end
    return M.ult(a, b)
end

function M.le(a: Int64, b: Int64): boolean
    return not M.lt(b, a)
end

function M.gt(a: Int64, b: Int64): boolean
    return M.lt(b, a)
end

function M.ge(a: Int64, b: Int64): boolean
    return not M.lt(a, b)
end

function M.isZero(a: Int64): boolean
    return a.lo == 0 and a.hi == 0
end

function M.isNegative(a: Int64): boolean
    return a.hi >= SIGN
end

-- Unsigned restoring division; carry retains the 65th bit of the shifted remainder.
local function divmod(a: Int64, b: Int64): (Int64, Int64)
    assert(not M.isZero(b), "Division by zero")
    local qLo, qHi, rLo, rHi = 0, 0, 0, 0
    for index = 63, 0, -1 do
        local carry = rHi >= SIGN
        local bit = if index >= 32 then bit32.extract(a.hi, index - 32) else bit32.extract(a.lo, index)
        rHi = (rHi * 2 + math.floor(rLo / SIGN)) % WORD
        rLo = (rLo * 2 + bit) % WORD
        if carry or rHi > b.hi or (rHi == b.hi and rLo >= b.lo) then
            local low = rLo - b.lo
            rHi = (rHi - b.hi - (if low < 0 then 1 else 0)) % WORD
            rLo = low % WORD
            if index >= 32 then
                qHi = bit32.bor(qHi, bit32.lshift(1, index - 32))
            else
                qLo = bit32.bor(qLo, bit32.lshift(1, index))
            end
        end
    end
    return M.fromWords(qLo, qHi), M.fromWords(rLo, rHi)
end

function M.udiv(a: Int64, b: Int64): Int64
    local quotient = divmod(a, b)
    return quotient
end

function M.urem(a: Int64, b: Int64): Int64
    local _, remainder = divmod(a, b)
    return remainder
end

function M.sdiv(a: Int64, b: Int64): Int64
    local aNegative, bNegative = M.isNegative(a), M.isNegative(b)
    local quotient = M.udiv(if aNegative then M.neg(a) else a, if bNegative then M.neg(b) else b)
    return if aNegative ~= bNegative then M.neg(quotient) else quotient
end

function M.srem(a: Int64, b: Int64): Int64
    local aNegative, bNegative = M.isNegative(a), M.isNegative(b)
    local remainder = M.urem(if aNegative then M.neg(a) else a, if bNegative then M.neg(b) else b)
    return if aNegative then M.neg(remainder) else remainder
end

function M.signExtend(val: Int64, bits: number): Int64
    local shift = 64 - bits
    return M.sar(M.shl(val, shift), shift)
end

function M.zeroExtend(val: Int64, bits: number): Int64
    if bits >= 64 then return val end
    if bits <= 32 then return M.fromWords(bit32.band(val.lo, 2 ^ bits - 1), 0) end
    return M.fromWords(val.lo, bit32.band(val.hi, 2 ^ (bits - 32) - 1))
end

function M.extractBits(val: Int64, lo: number, hi: number): Int64
    return M.zeroExtend(M.shr(val, lo), hi - lo + 1)
end

function M.clz64(val: Int64): number
    return if val.hi == 0 then 32 + bit32.countlz(val.lo) else bit32.countlz(val.hi)
end

local function rev32(val: number): number
    return bit32.bor(bit32.rshift(val, 24), bit32.band(bit32.rshift(val, 8), 0xFF00),
        bit32.band(bit32.lshift(val, 8), 0xFF0000), bit32.lshift(val, 24))
end

local function rbit32(val: number): number
    val = bit32.bor(bit32.band(bit32.rshift(val, 1), 0x55555555), bit32.lshift(bit32.band(val, 0x55555555), 1))
    val = bit32.bor(bit32.band(bit32.rshift(val, 2), 0x33333333), bit32.lshift(bit32.band(val, 0x33333333), 2))
    val = bit32.bor(bit32.band(bit32.rshift(val, 4), 0x0F0F0F0F), bit32.lshift(bit32.band(val, 0x0F0F0F0F), 4))
    return rev32(val)
end

function M.rbit64(val: Int64): Int64
    return M.fromWords(rbit32(val.hi), rbit32(val.lo))
end

function M.rev64(val: Int64): Int64
    return M.fromWords(rev32(val.hi), rev32(val.lo))
end

function M.rev32(val: Int64): Int64
    return M.fromWords(rev32(val.lo), 0)
end

function M.rev16(val: Int64): Int64
    local low = bit32.bor(bit32.band(bit32.rshift(val.lo, 8), 0x00FF00FF), bit32.lshift(bit32.band(val.lo, 0x00FF00FF), 8))
    return M.fromWords(low, 0)
end

return M
