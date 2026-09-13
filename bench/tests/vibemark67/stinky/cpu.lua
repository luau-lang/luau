--!strict

-- ARM64 CPU state: 31 general-purpose registers (X0-X30), SP, PC, and NZCV flags.
-- X30 is the link register (LR). Register index 31 in most encodings means either
-- SP or XZR depending on the instruction.

local Int = require("./integer")
local Memory = require("./memory")

local CPU = {}
CPU.__index = CPU

export type CPU = typeof(setmetatable({} :: {
    X: { integer },       -- X[0]..X[30], 1-indexed so X[1] = X0
    SP: integer,
    PC: integer,
    N: boolean,
    Z: boolean,
    C: boolean,
    V: boolean,
    mem: Memory.Memory,
    halted: boolean,
    exitCode: number,
    tpidr_el0: integer,
    V_lo: { integer },   -- V[0]..V[31] low 64 bits, 1-indexed
    V_hi: { integer },   -- V[0]..V[31] high 64 bits, 1-indexed
}, CPU))

function CPU.new(mem: Memory.Memory): CPU
    local regs: { integer } = {}
    for idx = 1, 31 do
        regs[idx] = Int.ZERO
    end
    local vlo: { integer } = {}
    local vhi: { integer } = {}
    for idx = 1, 32 do
        vlo[idx] = Int.ZERO
        vhi[idx] = Int.ZERO
    end
    local self = setmetatable({
        X = regs,
        SP = Int.ZERO,
        PC = Int.ZERO,
        N = false,
        Z = false,
        C = false,
        V = false,
        mem = mem,
        halted = false,
        exitCode = 0,
        tpidr_el0 = Int.ZERO,
        V_lo = vlo,
        V_hi = vhi,
    }, CPU)
    return self
end

-- Read register. Index 0-30 maps to X0-X30. Index 31 = XZR (reads as zero).
function CPU.readX(self: CPU, reg: number): integer
    if reg == 31 then return Int.ZERO end
    return self.X[reg + 1]
end

-- Write register. Index 31 = XZR (write is discarded).
function CPU.writeX(self: CPU, reg: number, val: integer)
    if reg == 31 then return end
    self.X[reg + 1] = val
end

-- Read as 32-bit (zero-extended).
function CPU.readW(self: CPU, reg: number): integer
    if reg == 31 then return Int.ZERO end
    return Int.band(self.X[reg + 1], Int.MASK32)
end

-- Write as 32-bit (zero-extended to 64 bits).
function CPU.writeW(self: CPU, reg: number, val: integer)
    if reg == 31 then return end
    self.X[reg + 1] = Int.band(val, Int.MASK32)
end

-- Read register or SP. In some encodings, reg 31 means SP instead of XZR.
function CPU.readXOrSP(self: CPU, reg: number): integer
    if reg == 31 then return self.SP end
    return self.X[reg + 1]
end

-- Write register or SP.
function CPU.writeXOrSP(self: CPU, reg: number, val: integer)
    if reg == 31 then
        self.SP = val
        return
    end
    self.X[reg + 1] = val
end

function CPU.readWOrSP(self: CPU, reg: number): integer
    if reg == 31 then return Int.band(self.SP, Int.MASK32) end
    return Int.band(self.X[reg + 1], Int.MASK32)
end

function CPU.writeWOrSP(self: CPU, reg: number, val: integer)
    if reg == 31 then
        self.SP = Int.band(val, Int.MASK32)
        return
    end
    self.X[reg + 1] = Int.band(val, Int.MASK32)
end

-- Update NZCV flags for a 64-bit result.
function CPU.setNZ64(self: CPU, result: integer)
    self.N = Int.isNegative(result)
    self.Z = Int.isZero(result)
end

-- Update NZCV flags for a 32-bit result.
function CPU.setNZ32(self: CPU, result: integer)
    local val = Int.band(result, Int.MASK32)
    self.N = Int.toNumber(Int.shr(val, 31)) == 1
    self.Z = Int.isZero(val)
end

-- Evaluate a condition code (0-15).
function CPU.evalCondition(self: CPU, cond: number): boolean
    local base = bit32.rshift(cond, 1)
    local result: boolean
    if base == 0 then     -- EQ/NE
        result = self.Z
    elseif base == 1 then -- CS/CC (HS/LO)
        result = self.C
    elseif base == 2 then -- MI/PL
        result = self.N
    elseif base == 3 then -- VS/VC
        result = self.V
    elseif base == 4 then -- HI/LS
        result = self.C and not self.Z
    elseif base == 5 then -- GE/LT
        result = (self.N == self.V)
    elseif base == 6 then -- GT/LE
        result = (not self.Z) and (self.N == self.V)
    else -- AL/NV
        result = true
    end
    -- Invert if low bit of cond is 1 (and cond != 15)
    if bit32.band(cond, 1) == 1 and cond ~= 15 then
        result = not result
    end
    return result
end

-- Add with carry for 64-bit, sets NZCV.
function CPU.addWithCarry64(self: CPU, a: integer, b: integer, carryIn: boolean): integer
    local carry: integer = if carryIn then Int.ONE else Int.ZERO
    local result = Int.add(Int.add(a, b), carry)
    self:setNZ64(result)

    -- Carry: unsigned overflow
    -- If result < a (unsigned), or result == a and carry was 1 and b was max
    if carryIn then
        self.C = Int.ule(result, a)
    else
        self.C = Int.ult(result, a)
    end

    -- Overflow: sign of result differs from what's expected
    local aSign = Int.isNegative(a)
    local bSign = Int.isNegative(b)
    local rSign = Int.isNegative(result)
    self.V = (aSign == bSign) and (rSign ~= aSign)

    return result
end

-- Add with carry for 32-bit, sets NZCV.
function CPU.addWithCarry32(self: CPU, a: integer, b: integer, carryIn: boolean): integer
    local a32 = Int.band(a, Int.MASK32)
    local b32 = Int.band(b, Int.MASK32)
    local carry: integer = if carryIn then Int.ONE else Int.ZERO

    -- Do the add in full 64-bit to detect carry
    local full = Int.add(Int.add(a32, b32), carry)
    local result = Int.band(full, Int.MASK32)

    self:setNZ32(result)

    -- Carry: result overflows 32 bits
    self.C = Int.ugt(full, Int.MASK32)

    -- Overflow: sign bit (bit 31) check
    local aSign = Int.toNumber(Int.shr(a32, 31)) == 1
    local bSign = Int.toNumber(Int.shr(b32, 31)) == 1
    local rSign = Int.toNumber(Int.shr(result, 31)) == 1
    self.V = (aSign == bSign) and (rSign ~= aSign)

    return result
end

-- SIMD/FP register access (128-bit as two 64-bit halves)
function CPU.readVLo(self: CPU, reg: number): integer
    return self.V_lo[reg + 1]
end

function CPU.readVHi(self: CPU, reg: number): integer
    return self.V_hi[reg + 1]
end

function CPU.writeV(self: CPU, reg: number, lo: integer, hi: integer)
    self.V_lo[reg + 1] = lo
    self.V_hi[reg + 1] = hi
end

function CPU.writeVLo(self: CPU, reg: number, lo: integer)
    self.V_lo[reg + 1] = lo
    self.V_hi[reg + 1] = Int.ZERO
end

function CPU.writeVFull(self: CPU, reg: number, lo: integer, hi: integer)
    self.V_lo[reg + 1] = lo
    self.V_hi[reg + 1] = hi
end

return CPU
