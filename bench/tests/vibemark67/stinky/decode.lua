--!strict

-- ARM64 instruction decoder and executor.
-- Decodes 32-bit instruction words and executes them on the CPU state.

local Int = require("./integer")
local CPU = require("./cpu")
local Syscall = require("./syscall")

local Decode = {}

-- Helper: sign-extend a number from `bits` width.
local function signExtendN(val: number, bits: number): number
    local mask = bit32.lshift(1, bits - 1)
    if bit32.band(val, mask) ~= 0 then
        -- negative: fill upper bits
        return val - bit32.lshift(1, bits)
    end
    return val
end

-- Helper: sign-extend to 64 bits from an immediate value of given bit width.
local function signExtendImm(val: number, bits: number): integer
    local sval = signExtendN(val, bits)
    if sval < 0 then
        return Int.add(Int.MAX_U64, Int.from(sval + 1))
    end
    return Int.from(sval)
end

-- Decode and replicate bitmask immediate (used by AND, ORR, EOR, etc.)
-- This is the trickiest encoding in ARM64.
local function decodeBitmaskImm(sf: number, immN: number, imms: number, immr: number): integer?
    local len: number
    -- Find the highest bit set in (N:NOT(imms))
    local combined = bit32.bor(bit32.lshift(immN, 6), bit32.band(bit32.bnot(imms), 0x3F))
    if combined == 0 then return nil end

    -- Find the length: position of highest set bit in combined
    len = 0
    for testBit = 6, 1, -1 do
        if bit32.band(combined, bit32.lshift(1, testBit)) ~= 0 then
            len = testBit
            break
        end
    end

    if len == 0 then return nil end

    local size = bit32.lshift(1, len) -- element size in bits
    local mask = bit32.band(imms, size - 1) -- extract S from lower bits of imms
    local levels = size - 1

    local s = bit32.band(imms, levels)
    local r = bit32.band(immr, levels)

    if s == levels then return nil end -- reserved

    -- Create the base pattern: (s+1) ones
    local ones = s + 1
    local pattern = Int.ZERO
    for idx = 0, ones - 1 do
        pattern = Int.bor(pattern, Int.shl(Int.ONE, idx))
    end

    -- Rotate right by r within the element
    if r ~= 0 then
        local elemMask = Int.sub(Int.shl(Int.ONE, size), Int.ONE)
        local rotated = Int.bor(
            Int.shr(Int.band(pattern, elemMask), r),
            Int.band(Int.shl(pattern, size - r), elemMask)
        )
        pattern = rotated
    end

    -- Replicate the element to fill the register width
    local regWidth = if sf == 1 then 64 else 32
    local result = Int.ZERO
    local pos = 0
    while pos < regWidth do
        result = Int.bor(result, Int.shl(pattern, pos))
        pos += size
    end

    if sf == 0 then
        result = Int.band(result, Int.MASK32)
    end

    return result
end

-- Decode the shift type (00=LSL, 01=LSR, 10=ASR, 11=ROR) and apply to value.
local function applyShift(val: integer, shiftType: number, amount: number, is32: boolean): integer
    if amount == 0 then return val end
    if is32 then
        val = Int.band(val, Int.MASK32)
    end
    if shiftType == 0 then -- LSL
        val = Int.shl(val, amount)
    elseif shiftType == 1 then -- LSR
        if is32 then
            val = Int.band(val, Int.MASK32)
        end
        val = Int.shr(val, amount)
    elseif shiftType == 2 then -- ASR
        if is32 then
            val = Int.signExtend(Int.band(val, Int.MASK32), 32)
        end
        val = Int.sar(val, amount)
    elseif shiftType == 3 then -- ROR
        local width = if is32 then 32 else 64
        local amt = amount % width
        if is32 then
            val = Int.band(val, Int.MASK32)
        end
        val = Int.bor(Int.shr(val, amt), Int.shl(val, width - amt))
    end
    if is32 then
        val = Int.band(val, Int.MASK32)
    end
    return val
end

-- Extend register value based on extend type.
local function applyExtend(val: integer, extType: number, shift: number): integer
    -- extType: 000=UXTB, 001=UXTH, 010=UXTW, 011=UXTX, 100=SXTB, 101=SXTH, 110=SXTW, 111=SXTX
    if extType == 0 then -- UXTB
        val = Int.band(val, Int.MASK8)
    elseif extType == 1 then -- UXTH
        val = Int.band(val, Int.MASK16)
    elseif extType == 2 then -- UXTW
        val = Int.band(val, Int.MASK32)
    elseif extType == 3 then -- UXTX
        -- no-op, full 64-bit
    elseif extType == 4 then -- SXTB
        val = Int.signExtend(Int.band(val, Int.MASK8), 8)
    elseif extType == 5 then -- SXTH
        val = Int.signExtend(Int.band(val, Int.MASK16), 16)
    elseif extType == 6 then -- SXTW
        val = Int.signExtend(Int.band(val, Int.MASK32), 32)
    elseif extType == 7 then -- SXTX
        -- no-op
    end
    if shift > 0 then
        val = Int.shl(val, shift)
    end
    return val
end

-- FP bit conversion helpers
local fpBuf = buffer.create(8)

local function f64ToInt(val: number): integer
    buffer.writef64(fpBuf, 0, val)
    return buffer.readinteger(fpBuf, 0, 8)
end

local function intToF64(bits: integer): number
    buffer.writeinteger(fpBuf, 0, bits, 8)
    return buffer.readf64(fpBuf, 0)
end

local function f32ToInt(val: number): number
    buffer.writef32(fpBuf, 0, val)
    return buffer.readu32(fpBuf, 0)
end

local function intToF32(bits: number): number
    buffer.writeu32(fpBuf, 0, bits)
    return buffer.readf32(fpBuf, 0)
end

local function readFPD(cpu: CPU.CPU, reg: number): number
    return intToF64(cpu:readVLo(reg))
end

local function writeFPD(cpu: CPU.CPU, reg: number, val: number)
    cpu:writeVFull(reg, f64ToInt(val), Int.ZERO)
end

local function readFPS(cpu: CPU.CPU, reg: number): number
    return intToF32(Int.toNumber(Int.band(cpu:readVLo(reg), Int.MASK32)))
end

local function writeFPS(cpu: CPU.CPU, reg: number, val: number)
    cpu:writeVFull(reg, Int.from(f32ToInt(val)), Int.ZERO)
end

-- Main execution function. Returns true if execution should continue.
function Decode.step(cpu: CPU.CPU): boolean
    if cpu.halted then return false end

    local pc = cpu.PC
    local insn = cpu.mem:readU32(pc)
    cpu.PC = Int.add(pc, Int.from(4))

    -- Top-level decode based on bits [28:25] (the "op1" field in ARM ARM)
    local op1 = bit32.band(bit32.rshift(insn, 25), 0xF)

    -- Data Processing Immediate: op1 = 100x
    if bit32.band(op1, 0xE) == 0x8 then
        Decode.execDPImm(cpu, insn, pc)
        return not cpu.halted
    end

    -- Branch/System: op1 = 101x
    if bit32.band(op1, 0xE) == 0xA then
        Decode.execBranch(cpu, insn, pc)
        return not cpu.halted
    end

    -- Loads and Stores: op1 = x1x0
    if bit32.band(op1, 0x5) == 0x4 then
        Decode.execLoadStore(cpu, insn, pc)
        return not cpu.halted
    end

    -- Data Processing Register (and SIMD): op1 = x101
    if bit32.band(op1, 0x5) == 0x5 then
        Decode.execDPReg(cpu, insn, pc)
        return not cpu.halted
    end

    -- Remaining: op1 = 0xx0 where bit[27]=0 (SVE, SME, or unallocated)
    -- Handle as unimplemented
    Decode.unimplemented(cpu, insn, pc)
    return false
end

function Decode.unimplemented(cpu: CPU.CPU, insn: number, pc: integer)
    local pcNum = Int.toNumber(pc)
    error(string.format("Unimplemented instruction: 0x%08x at PC=0x%x", insn, pcNum))
end

------------------------------------------------------------
-- Data Processing - Immediate
------------------------------------------------------------
function Decode.execDPImm(cpu: CPU.CPU, insn: number, pc: integer)
    local op0 = bit32.band(bit32.rshift(insn, 23), 0x7) -- bits [25:23]

    if op0 == 0 or op0 == 1 then
        -- PC-rel addressing: ADR/ADRP
        Decode.execPCRel(cpu, insn, pc)
    elseif op0 == 2 or op0 == 3 then
        -- Add/Sub immediate
        Decode.execAddSubImm(cpu, insn, pc)
    elseif op0 == 4 then
        -- Logical immediate
        Decode.execLogicalImm(cpu, insn, pc)
    elseif op0 == 5 then
        -- Move wide immediate
        Decode.execMoveWide(cpu, insn, pc)
    elseif op0 == 6 then
        -- Bitfield
        Decode.execBitfield(cpu, insn, pc)
    elseif op0 == 7 then
        -- Extract
        Decode.execExtract(cpu, insn, pc)
    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execPCRel(cpu: CPU.CPU, insn: number, pc: integer)
    local rd = bit32.band(insn, 0x1F)
    local immLo = bit32.band(bit32.rshift(insn, 29), 0x3)
    local immHi = bit32.band(bit32.rshift(insn, 5), 0x7FFFF)
    local op = bit32.band(bit32.rshift(insn, 31), 1)

    local imm = bit32.bor(bit32.lshift(immHi, 2), immLo) -- 21-bit value
    local offset = signExtendImm(imm, 21)

    if op == 0 then
        -- ADR: PC + offset
        cpu:writeX(rd, Int.add(pc, offset))
    else
        -- ADRP: (PC & ~0xFFF) + (offset << 12)
        local pageMask = Int.bnot(Int.from(0xFFF))
        local base = Int.band(pc, pageMask)
        cpu:writeX(rd, Int.add(base, Int.shl(offset, 12)))
    end
end

function Decode.execAddSubImm(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1)     -- 0=ADD, 1=SUB
    local setFlags = bit32.band(bit32.rshift(insn, 29), 1) == 1
    local shift = bit32.band(bit32.rshift(insn, 22), 0x3) -- 0=none, 1=LSL#12
    local imm12 = bit32.band(bit32.rshift(insn, 10), 0xFFF)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local immVal = Int.from(imm12)
    if shift == 1 then
        immVal = Int.shl(immVal, 12)
    end

    local operand1 = if sf == 1 then cpu:readXOrSP(rn) else cpu:readWOrSP(rn)

    local result: integer
    if op == 0 then -- ADD
        if setFlags then
            if sf == 1 then
                result = cpu:addWithCarry64(operand1, immVal, false)
            else
                result = cpu:addWithCarry32(operand1, immVal, false)
            end
        else
            result = Int.add(operand1, immVal)
            if sf == 0 then result = Int.band(result, Int.MASK32) end
        end
    else -- SUB
        local negImm = if sf == 1 then Int.bnot(immVal) else Int.band(Int.bnot(immVal), Int.MASK32)
        if setFlags then
            if sf == 1 then
                result = cpu:addWithCarry64(operand1, negImm, true)
            else
                result = cpu:addWithCarry32(operand1, negImm, true)
            end
        else
            result = Int.sub(operand1, immVal)
            if sf == 0 then result = Int.band(result, Int.MASK32) end
        end
    end

    if setFlags then
        -- CMP/CMN use Rd=31 (XZR) to discard result
        if rd ~= 31 then
            if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
        end
    else
        -- Non-flag-setting variants use SP encoding for Rd
        if sf == 1 then cpu:writeXOrSP(rd, result) else cpu:writeWOrSP(rd, result) end
    end
end

function Decode.execLogicalImm(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local opc = bit32.band(bit32.rshift(insn, 29), 0x3)
    local immN = bit32.band(bit32.rshift(insn, 22), 1)
    local immr = bit32.band(bit32.rshift(insn, 16), 0x3F)
    local imms = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local imm = decodeBitmaskImm(sf, immN, imms, immr)
    if imm == nil then
        Decode.unimplemented(cpu, insn, pc)
        return
    end

    local operand1 = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local result: integer

    if opc == 0 then -- AND
        result = Int.band(operand1, imm)
    elseif opc == 1 then -- ORR
        result = Int.bor(operand1, imm)
    elseif opc == 2 then -- EOR
        result = Int.bxor(operand1, imm)
    else -- ANDS (3)
        result = Int.band(operand1, imm)
        if sf == 1 then cpu:setNZ64(result) else cpu:setNZ32(result) end
        cpu.C = false
        cpu.V = false
    end

    if sf == 0 then result = Int.band(result, Int.MASK32) end

    if opc == 3 then
        -- ANDS: rd is XZR-encoding (flag-setting)
        if rd ~= 31 then
            if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
        end
    else
        -- SP-encoding for rd
        if sf == 1 then cpu:writeXOrSP(rd, result) else cpu:writeWOrSP(rd, result) end
    end
end

function Decode.execMoveWide(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local opc = bit32.band(bit32.rshift(insn, 29), 0x3)
    local hw = bit32.band(bit32.rshift(insn, 21), 0x3)
    local imm16 = bit32.band(bit32.rshift(insn, 5), 0xFFFF)
    local rd = bit32.band(insn, 0x1F)

    local shift = hw * 16
    local val = Int.shl(Int.from(imm16), shift)

    if opc == 0 then -- MOVN
        val = Int.bnot(val)
        if sf == 0 then val = Int.band(val, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, val) else cpu:writeW(rd, val) end
    elseif opc == 2 then -- MOVZ
        if sf == 0 then val = Int.band(val, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, val) else cpu:writeW(rd, val) end
    elseif opc == 3 then -- MOVK
        local existing = if sf == 1 then cpu:readX(rd) else cpu:readW(rd)
        -- Clear the 16-bit slot, then OR in new value
        local mask = Int.bnot(Int.shl(Int.from(0xFFFF), shift))
        local result = Int.bor(Int.band(existing, mask), Int.shl(Int.from(imm16), shift))
        if sf == 0 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execBitfield(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local opc = bit32.band(bit32.rshift(insn, 29), 0x3)
    local immr = bit32.band(bit32.rshift(insn, 16), 0x3F)
    local imms = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local regWidth = if sf == 1 then 64 else 32
    local src = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)

    if opc == 0 then -- SBFM (signed bitfield move)
        -- Handles: ASR, SBFIZ, SBFX, SXTB, SXTH, SXTW
        local result: integer
        if imms >= immr then
            -- Extract bits [imms:immr] and sign-extend from bit (imms-immr)
            local width = imms - immr + 1
            local extracted = Int.band(Int.shr(src, immr), Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            result = Int.signExtend(extracted, width)
        else
            -- Shift/rotate left and sign-extend
            local width = imms + 1
            local extracted = Int.band(src, Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            local pos = regWidth - immr
            result = Int.shl(extracted, pos)
            result = Int.signExtend(result, pos + width)
        end
        if sf == 0 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end

    elseif opc == 1 then -- BFM (bitfield move)
        -- Handles: BFI, BFXIL
        local dst = if sf == 1 then cpu:readX(rd) else cpu:readW(rd)
        local result: integer
        if imms >= immr then
            local width = imms - immr + 1
            local extracted = Int.band(Int.shr(src, immr), Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            -- Insert at bit 0
            local mask = Int.sub(Int.shl(Int.ONE, width), Int.ONE)
            result = Int.bor(Int.band(dst, Int.bnot(mask)), extracted)
        else
            local width = imms + 1
            local extracted = Int.band(src, Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            local pos = regWidth - immr
            local mask = Int.shl(Int.sub(Int.shl(Int.ONE, width), Int.ONE), pos)
            result = Int.bor(Int.band(dst, Int.bnot(mask)), Int.shl(extracted, pos))
        end
        if sf == 0 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end

    elseif opc == 2 then -- UBFM (unsigned bitfield move)
        -- Handles: LSL, LSR, UBFIZ, UBFX, UXTB, UXTH
        local result: integer
        if imms >= immr then
            local width = imms - immr + 1
            local extracted = Int.band(Int.shr(src, immr), Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            result = extracted
        else
            local width = imms + 1
            local extracted = Int.band(src, Int.sub(Int.shl(Int.ONE, width), Int.ONE))
            local pos = regWidth - immr
            result = Int.shl(extracted, pos)
        end
        if sf == 0 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end

    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execExtract(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local imms = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local regWidth = if sf == 1 then 64 else 32
    local hi = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local lo = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)

    -- EXTR: Rd = (Rn:Rm) >> lsb
    local lsb = imms
    local result: integer
    if lsb == 0 then
        result = lo
    else
        result = Int.bor(Int.shr(lo, lsb), Int.shl(hi, regWidth - lsb))
    end
    if sf == 0 then result = Int.band(result, Int.MASK32) end
    if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
end

------------------------------------------------------------
-- Branches and System
------------------------------------------------------------
function Decode.execBranch(cpu: CPU.CPU, insn: number, pc: integer)
    local op0 = bit32.band(bit32.rshift(insn, 29), 0x7) -- bits [31:29]
    local op1 = bit32.band(bit32.rshift(insn, 25), 0x3) -- bits [26:25] within this group

    -- Unconditional branch immediate: B and BL
    -- B:  0 00101 imm26  (bit31=0)
    -- BL: 1 00101 imm26  (bit31=1)
    if bit32.band(insn, 0xFC000000) == 0x14000000 then
        -- B
        local imm26 = bit32.band(insn, 0x3FFFFFF)
        local offset = signExtendImm(imm26, 26)
        cpu.PC = Int.add(pc, Int.shl(offset, 2))
        return
    end

    if bit32.band(insn, 0xFC000000) == 0x94000000 then
        -- BL
        local imm26 = bit32.band(insn, 0x3FFFFFF)
        local offset = signExtendImm(imm26, 26)
        cpu:writeX(30, Int.add(pc, Int.from(4))) -- LR = next insn
        cpu.PC = Int.add(pc, Int.shl(offset, 2))
        return
    end

    -- Conditional branch
    if bit32.band(insn, 0xFE000000) == 0x54000000 then
        local imm19 = bit32.band(bit32.rshift(insn, 5), 0x7FFFF)
        local cond = bit32.band(insn, 0xF)
        if cpu:evalCondition(cond) then
            local offset = signExtendImm(imm19, 19)
            cpu.PC = Int.add(pc, Int.shl(offset, 2))
        end
        return
    end

    -- CBZ/CBNZ
    if bit32.band(insn, 0x7E000000) == 0x34000000 then
        local sf = bit32.band(bit32.rshift(insn, 31), 1)
        local op = bit32.band(bit32.rshift(insn, 24), 1) -- 0=CBZ, 1=CBNZ
        local imm19 = bit32.band(bit32.rshift(insn, 5), 0x7FFFF)
        local rt = bit32.band(insn, 0x1F)
        local val = if sf == 1 then cpu:readX(rt) else cpu:readW(rt)
        local isZero = Int.isZero(val)
        if (op == 0 and isZero) or (op == 1 and not isZero) then
            local offset = signExtendImm(imm19, 19)
            cpu.PC = Int.add(pc, Int.shl(offset, 2))
        end
        return
    end

    -- TBZ/TBNZ
    if bit32.band(insn, 0x7E000000) == 0x36000000 then
        local b5 = bit32.band(bit32.rshift(insn, 31), 1)
        local op = bit32.band(bit32.rshift(insn, 24), 1) -- 0=TBZ, 1=TBNZ
        local b40 = bit32.band(bit32.rshift(insn, 19), 0x1F)
        local bitNum = bit32.bor(bit32.lshift(b5, 5), b40)
        local imm14 = bit32.band(bit32.rshift(insn, 5), 0x3FFF)
        local rt = bit32.band(insn, 0x1F)
        local val = cpu:readX(rt)
        local testBit = Int.band(Int.shr(val, bitNum), Int.ONE)
        local isSet = not Int.isZero(testBit)
        if (op == 0 and not isSet) or (op == 1 and isSet) then
            local offset = signExtendImm(imm14, 14)
            cpu.PC = Int.add(pc, Int.shl(offset, 2))
        end
        return
    end

    -- Exception generation (SVC, BRK, HLT)
    if bit32.band(insn, 0xFF000000) == 0xD4000000 then
        if bit32.band(insn, 0xFFE0001F) == 0xD4000001 then -- SVC
            Syscall.handle(cpu)
        elseif bit32.band(insn, 0xFFE0001F) == 0xD4200000 then -- BRK
            local imm16 = bit32.band(bit32.rshift(insn, 5), 0xFFFF)
            error(string.format("BRK #%d at PC=0x%x", imm16, Int.toNumber(pc)))
        end
        -- HLT and others: ignore
        return
    end

    -- Unconditional branch register (BR, BLR, RET)
    if bit32.band(insn, 0xFE000000) == 0xD6000000 then
        local opc = bit32.band(bit32.rshift(insn, 21), 0xF)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        if opc == 0 then -- BR
            cpu.PC = cpu:readX(rn)
        elseif opc == 1 then -- BLR
            cpu:writeX(30, Int.add(pc, Int.from(4)))
            cpu.PC = cpu:readX(rn)
        elseif opc == 2 then -- RET
            cpu.PC = cpu:readX(rn)
        else
            Decode.unimplemented(cpu, insn, pc)
        end
        return
    end

    -- System instructions (MSR, MRS, SVC, NOP, DMB, etc.)
    if bit32.band(insn, 0xFFC00000) == 0xD5000000 then
        Decode.execSystem(cpu, insn, pc)
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSystem(cpu: CPU.CPU, insn: number, pc: integer)
    -- SVC
    if bit32.band(insn, 0xFFE0001F) == 0xD4000001 then
        Syscall.handle(cpu)
        return
    end

    -- NOP (and other hints)
    if bit32.band(insn, 0xFFFFF01F) == 0xD503201F then
        return -- NOP, YIELD, WFE, WFI, SEV, etc. - treat as NOP
    end

    -- DMB, DSB, ISB - memory barriers, treat as NOP
    if bit32.band(insn, 0xFFFFF0FF) == 0xD503309F or -- DSB
       bit32.band(insn, 0xFFFFF0FF) == 0xD50330BF or -- DMB
       bit32.band(insn, 0xFFFFF0FF) == 0xD50330DF then -- ISB
        return
    end

    -- MRS
    if bit32.band(insn, 0xFFF00000) == 0xD5300000 then
        local rt = bit32.band(insn, 0x1F)
        local sysReg = bit32.band(bit32.rshift(insn, 5), 0x7FFF)
        -- TPIDR_EL0: op0=3, op1=3, CRn=13, CRm=0, op2=2 -> encoded as 0xDE82
        -- DCZID_EL0: op0=3, op1=3, CRn=0, CRm=0, op2=7 -> encoded as 0xD807 (approx)
        -- Actually let's just decode the full system register encoding
        local op0sys = bit32.band(bit32.rshift(insn, 19), 0x3)
        local op1sys = bit32.band(bit32.rshift(insn, 16), 0x7)
        local crn = bit32.band(bit32.rshift(insn, 12), 0xF)
        local crm = bit32.band(bit32.rshift(insn, 8), 0xF)
        local op2sys = bit32.band(bit32.rshift(insn, 5), 0x7)

        if op0sys == 3 and op1sys == 3 and crn == 13 and crm == 0 and op2sys == 2 then
            -- TPIDR_EL0
            cpu:writeX(rt, cpu.tpidr_el0)
        elseif op0sys == 3 and op1sys == 3 and crn == 0 and crm == 0 and op2sys == 7 then
            -- DCZID_EL0 - data cache zero ID register
            -- bit4=1 means DC ZVA is prohibited, let's say cache line = 64 bytes (log2(64/4)=4)
            cpu:writeX(rt, Int.from(0x10)) -- DZP=1 (prohibited)
        elseif op0sys == 3 and op1sys == 3 and crn == 14 and crm == 0 and op2sys == 1 then
            -- CNTPCT_EL0 - counter timer
            cpu:writeX(rt, Int.ZERO)
        else
            -- Unknown system register - return 0
            cpu:writeX(rt, Int.ZERO)
        end
        return
    end

    -- MSR
    if bit32.band(insn, 0xFFF00000) == 0xD5100000 then
        local rt = bit32.band(insn, 0x1F)
        local op0sys = bit32.band(bit32.rshift(insn, 19), 0x3)
        local op1sys = bit32.band(bit32.rshift(insn, 16), 0x7)
        local crn = bit32.band(bit32.rshift(insn, 12), 0xF)
        local crm = bit32.band(bit32.rshift(insn, 8), 0xF)
        local op2sys = bit32.band(bit32.rshift(insn, 5), 0x7)

        if op0sys == 3 and op1sys == 3 and crn == 13 and crm == 0 and op2sys == 2 then
            -- TPIDR_EL0
            cpu.tpidr_el0 = cpu:readX(rt)
        end
        -- Otherwise ignore
        return
    end

    -- BTI, PACIASP, AUTIASP, etc. - treat as NOP
    if bit32.band(insn, 0xFFFFFC00) == 0xD503241F or -- BTI variants
       bit32.band(insn, 0xFFFFFFFF) == 0xD503233F or -- PACIASP
       bit32.band(insn, 0xFFFFFFFF) == 0xD50323BF or -- AUTIASP
       bit32.band(insn, 0xFFFFFFFF) == 0xD503219F or -- AUTIA1716
       bit32.band(insn, 0xFFFFFFFF) == 0xD503211F then -- AUTIB1716
        return
    end

    -- CLREX, other system ops - NOP
    if bit32.band(insn, 0xFFFFF0FF) == 0xD503305F then -- CLREX
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

------------------------------------------------------------
-- Load/Store
------------------------------------------------------------
function Decode.execLoadStore(cpu: CPU.CPU, insn: number, pc: integer)
    local vBit = bit32.band(bit32.rshift(insn, 26), 1) -- V flag: 1 = SIMD/FP

    -- Load/store pair (LDP, STP) - both integer and SIMD
    if bit32.band(insn, 0x3A000000) == 0x28000000 then
        if vBit == 1 then
            Decode.execLoadStorePairSimd(cpu, insn, pc)
        else
            Decode.execLoadStorePair(cpu, insn, pc)
        end
        return
    end

    -- Load/store register (various addressing modes) - both integer and SIMD
    if bit32.band(insn, 0x3B000000) == 0x38000000 then
        if vBit == 1 then
            Decode.execLoadStoreRegSimd(cpu, insn, pc)
        else
            Decode.execLoadStoreReg(cpu, insn, pc)
        end
        return
    end

    if bit32.band(insn, 0x3B000000) == 0x39000000 then
        if vBit == 1 then
            Decode.execLoadStoreUnsignedOffSimd(cpu, insn, pc)
        else
            Decode.execLoadStoreUnsignedOff(cpu, insn, pc)
        end
        return
    end

    -- Load/store exclusive, ordered, etc.
    if bit32.band(insn, 0x3F000000) == 0x08000000 then
        Decode.execLoadStoreExclusive(cpu, insn, pc)
        return
    end

    -- Atomic memory operations (LDADD, SWP, CAS, etc.)
    if bit32.band(insn, 0x3B200C00) == 0x38200000 then
        Decode.execAtomic(cpu, insn, pc)
        return
    end

    -- Load register literal (LDR literal)
    if bit32.band(insn, 0x3B000000) == 0x18000000 then
        Decode.execLoadLiteral(cpu, insn, pc)
        return
    end

    -- PRFM (prefetch) - NOP
    if bit32.band(insn, 0xFFC00000) == 0xF9800000 or -- PRFM unsigned offset
       bit32.band(insn, 0xFFE00C00) == 0xF8A00000 then -- PRFM register
        return
    end

    -- MTE instructions (STG, LDG, etc.) - treat as NOP
    if bit32.band(insn, 0xFFE00C00) == 0xD9200800 or -- STG
       bit32.band(insn, 0xFFE00C00) == 0xD9600800 or -- ST2G
       bit32.band(insn, 0xFFE00C00) == 0xD9200C00 or -- STG pre-index
       bit32.band(insn, 0xFFE00C00) == 0xD9600C00 or -- ST2G pre-index
       bit32.band(insn, 0xFFE00C00) == 0xD9200400 or -- STG post-index
       bit32.band(insn, 0xFFE00C00) == 0xD9A00800 or -- STZG
       bit32.band(insn, 0xFFE00C00) == 0xD9E00800 or -- STZ2G
       bit32.band(insn, 0xFFE00C00) == 0xD9E00C00 or -- STZ2G pre
       bit32.band(insn, 0xFFE00C00) == 0xD9A00C00 then -- STZG pre
        return
    end
    if bit32.band(insn, 0xFFE00C00) == 0xD9600000 then -- LDG
        local rt = bit32.band(insn, 0x1F)
        cpu:writeX(rt, Int.ZERO)
        return
    end

    -- IRG, GMI - MTE tag instructions, treat as NOP/passthrough
    if bit32.band(insn, 0xFFE0FC00) == 0x9AC01400 then -- IRG
        local rd = bit32.band(insn, 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        cpu:writeXOrSP(rd, cpu:readXOrSP(rn))
        return
    end
    if bit32.band(insn, 0xFFE0FC00) == 0x9AC01000 then -- GMI
        local rd = bit32.band(insn, 0x1F)
        cpu:writeX(rd, Int.ZERO)
        return
    end

    -- DC ZVA/GVA - data cache zero by VA - actually zeros memory
    if bit32.band(insn, 0xFFFFFFE0) == 0xD50B7420 or -- DC ZVA
       bit32.band(insn, 0xFFFFFFE0) == 0xD50B7400 then -- DC IVAC etc
        return -- treat as NOP since DCZID says prohibited
    end

    -- Advanced SIMD load/store multiple structures (LD1, ST1, LD2, ST2, etc.)
    -- Encoding: 0 Q 001100 0 L 0 Rm/00000 opcode size Rn Rt
    if bit32.band(insn, 0xBF000000) == 0x0C000000 then
        Decode.execSimdLdSt(cpu, insn, pc)
        return
    end

    -- Advanced SIMD load/store single structure (LD1 {Vt}[index], etc.)
    if bit32.band(insn, 0xBF800000) == 0x0D000000 then
        Decode.execSimdLdStSingle(cpu, insn, pc)
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execLoadStorePair(cpu: CPU.CPU, insn: number, pc: integer)
    local opc = bit32.band(bit32.rshift(insn, 30), 0x3)
    local isLoad = bit32.band(bit32.rshift(insn, 22), 1) == 1
    local indexMode = bit32.band(bit32.rshift(insn, 23), 0x7) -- bits [25:23]
    local imm7 = bit32.band(bit32.rshift(insn, 15), 0x7F)
    local rt2 = bit32.band(bit32.rshift(insn, 10), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local scale = if opc == 0 then 2 elseif opc == 1 then 2 else 3 -- 2 for 32-bit, 3 for 64-bit
    -- opc: 00 = W (32-bit), 01 = LDPSW (signed 32->64), 10 = X (64-bit)
    local is64 = (opc == 2)
    local isSigned = (opc == 1)
    local dataSize = if is64 then 8 else 4

    local offset = signExtendN(imm7, 7) * dataSize

    local base = cpu:readXOrSP(rn)
    local wback: integer = base
    local addr: integer

    -- indexMode: 001 = post-index, 011 = pre-index, 010 = signed offset
    local mode = bit32.band(bit32.rshift(insn, 23), 0x7)
    if mode == 1 then -- post-index (no-alloc pair uses 0, treat similarly)
        addr = base
        wback = Int.add(base, Int.from(offset))
    elseif mode == 3 then -- pre-index
        addr = Int.add(base, Int.from(offset))
        wback = addr
    elseif mode == 2 or mode == 0 then -- signed offset (or no-alloc)
        addr = Int.add(base, Int.from(offset))
        wback = base -- no writeback
    else
        addr = Int.add(base, Int.from(offset))
        wback = base
    end

    if isLoad then
        if is64 then
            local val1 = cpu.mem:readU64(addr)
            local val2 = cpu.mem:readU64(Int.add(addr, Int.from(8)))
            cpu:writeX(rt, val1)
            cpu:writeX(rt2, val2)
        else
            local val1: integer
            local val2: integer
            if isSigned then
                val1 = Int.signExtend(Int.from(cpu.mem:readU32(addr)), 32)
                val2 = Int.signExtend(Int.from(cpu.mem:readU32(Int.add(addr, Int.from(4)))), 32)
            else
                val1 = Int.from(cpu.mem:readU32(addr))
                val2 = Int.from(cpu.mem:readU32(Int.add(addr, Int.from(4))))
            end
            cpu:writeX(rt, val1)
            cpu:writeX(rt2, val2)
        end
    else
        if is64 then
            cpu.mem:writeU64(addr, cpu:readX(rt))
            cpu.mem:writeU64(Int.add(addr, Int.from(8)), cpu:readX(rt2))
        else
            cpu.mem:writeU32(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK32)))
            cpu.mem:writeU32(Int.add(addr, Int.from(4)), Int.toNumber(Int.band(cpu:readX(rt2), Int.MASK32)))
        end
    end

    -- Writeback
    if mode == 1 or mode == 3 then
        cpu:writeXOrSP(rn, wback)
    end
end

function Decode.execLoadStoreReg(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3) -- 00=8, 01=16, 10=32, 11=64 bit
    local opc = bit32.band(bit32.rshift(insn, 22), 0x3) -- load/store/sign-extend variants
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local op4 = bit32.band(bit32.rshift(insn, 10), 0x3) -- bits [11:10]
    local op3 = bit32.band(bit32.rshift(insn, 21), 1)    -- bit [21]

    local base = cpu:readXOrSP(rn)
    local addr: integer
    local doWriteback = false
    local wbackAddr: integer = base

    if op3 == 1 and op4 == 2 then
        -- Register offset: size:opc [31:30,23:22] + Rm extended/shifted
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local option = bit32.band(bit32.rshift(insn, 13), 0x7)
        local s = bit32.band(bit32.rshift(insn, 12), 1)
        local shift = if s == 1 then size else 0
        local offset = applyExtend(cpu:readX(rm), option, shift)
        addr = Int.add(base, offset)
    elseif op4 == 0 then
        -- Unscaled immediate (LDUR/STUR)
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        local offset = signExtendImm(imm9, 9)
        addr = Int.add(base, offset)
    elseif op4 == 1 then
        -- Post-index
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        local offset = signExtendImm(imm9, 9)
        addr = base
        wbackAddr = Int.add(base, offset)
        doWriteback = true
    elseif op4 == 3 then
        -- Pre-index
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        local offset = signExtendImm(imm9, 9)
        addr = Int.add(base, offset)
        wbackAddr = addr
        doWriteback = true
    else
        -- op4 == 2 with op3 == 0: unprivileged or other
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        local offset = signExtendImm(imm9, 9)
        addr = Int.add(base, offset)
    end

    Decode.doLoadStore(cpu, size, opc, rt, addr)

    if doWriteback then
        cpu:writeXOrSP(rn, wbackAddr)
    end
end

function Decode.execLoadStoreUnsignedOff(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3)
    local opc = bit32.band(bit32.rshift(insn, 22), 0x3)
    local imm12 = bit32.band(bit32.rshift(insn, 10), 0xFFF)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local scale = size -- byte=0, half=1, word=2, dword=3
    local offset = imm12 * bit32.lshift(1, scale)

    local base = cpu:readXOrSP(rn)
    local addr = Int.add(base, Int.from(offset))

    Decode.doLoadStore(cpu, size, opc, rt, addr)
end

function Decode.doLoadStore(cpu: CPU.CPU, size: number, opc: number, rt: number, addr: integer)
    -- size: 0=byte, 1=half, 2=word, 3=dword
    -- opc: 0=store, 1=load(zero-extend), 2=load(sign-extend to 64), 3=load(sign-extend to 32) or PRFM
    if opc == 0 then
        -- Store
        if size == 0 then
            cpu.mem:writeU8(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK8)))
        elseif size == 1 then
            cpu.mem:writeU16(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK16)))
        elseif size == 2 then
            cpu.mem:writeU32(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK32)))
        else
            cpu.mem:writeU64(addr, cpu:readX(rt))
        end
    elseif opc == 1 then
        -- Load zero-extend
        if size == 0 then
            cpu:writeX(rt, Int.from(cpu.mem:readU8(addr)))
        elseif size == 1 then
            cpu:writeX(rt, Int.from(cpu.mem:readU16(addr)))
        elseif size == 2 then
            cpu:writeX(rt, Int.from(cpu.mem:readU32(addr)))
        else
            cpu:writeX(rt, cpu.mem:readU64(addr))
        end
    elseif opc == 2 then
        -- Load sign-extend to 64-bit
        if size == 0 then
            cpu:writeX(rt, Int.signExtend(Int.from(cpu.mem:readU8(addr)), 8))
        elseif size == 1 then
            cpu:writeX(rt, Int.signExtend(Int.from(cpu.mem:readU16(addr)), 16))
        elseif size == 2 then
            cpu:writeX(rt, Int.signExtend(Int.from(cpu.mem:readU32(addr)), 32))
        else
            -- PRFM for size==3, opc==2 — treat as NOP
            return
        end
    elseif opc == 3 then
        -- Load sign-extend to 32-bit (only for size 0,1) or PRFM (size 2,3)
        if size == 0 then
            cpu:writeW(rt, Int.signExtend(Int.from(cpu.mem:readU8(addr)), 8))
        elseif size == 1 then
            cpu:writeW(rt, Int.signExtend(Int.from(cpu.mem:readU16(addr)), 16))
        else
            -- PRFM - NOP
            return
        end
    end
end

-- Decode SIMD modified immediate value: returns a 64-bit value that gets replicated
function Decode.decodeSIMDModifiedImm(imm8: number, cmode: number, op: number): integer
    local cmodeHigh = bit32.rshift(cmode, 1)
    if cmodeHigh == 0 then -- 32-bit, no shift
        local w = imm8
        return Int.bor(Int.from(w), Int.shl(Int.from(w), 32))
    elseif cmodeHigh == 1 then -- 32-bit, LSL #8
        local w = imm8 * 256
        return Int.bor(Int.from(w), Int.shl(Int.from(w), 32))
    elseif cmodeHigh == 2 then -- 32-bit, LSL #16
        local w = imm8 * 65536
        return Int.bor(Int.from(w), Int.shl(Int.from(w), 32))
    elseif cmodeHigh == 3 then -- 32-bit, LSL #24
        local w = imm8 * 16777216
        return Int.bor(Int.from(w), Int.shl(Int.from(w), 32))
    elseif cmodeHigh == 4 then -- 16-bit, no shift
        local h = Int.from(imm8)
        local result = Int.ZERO
        for s = 0, 48, 16 do result = Int.bor(result, Int.shl(h, s)) end
        return result
    elseif cmodeHigh == 5 then -- 16-bit, LSL #8
        local h = Int.from(imm8 * 256)
        local result = Int.ZERO
        for s = 0, 48, 16 do result = Int.bor(result, Int.shl(h, s)) end
        return result
    elseif cmodeHigh == 6 then -- 32-bit, MSL #8 or MSL #16
        local shift = if bit32.band(cmode, 1) == 0 then 8 else 16
        local mask = bit32.lshift(1, shift) - 1
        local w = bit32.bor(bit32.lshift(imm8, shift), mask)
        return Int.bor(Int.from(w), Int.shl(Int.from(w), 32))
    else -- cmodeHigh == 7
        if op == 0 and bit32.band(cmode, 1) == 0 then
            -- 8-bit: replicate byte
            local result = Int.ZERO
            local b = Int.from(imm8)
            for s = 0, 56, 8 do
                result = Int.bor(result, Int.shl(b, s))
            end
            return result
        elseif op == 0 and bit32.band(cmode, 1) == 1 then
            -- 64-bit: each bit of imm8 selects 0x00 or 0xFF for corresponding byte
            local result = Int.ZERO
            for bit = 0, 7 do
                if bit32.band(imm8, bit32.lshift(1, bit)) ~= 0 then
                    result = Int.bor(result, Int.shl(Int.from(0xFF), bit * 8))
                end
            end
            return result
        else
            -- FMOV (FP immediate) - return as 64-bit pattern
            return Int.from(imm8)
        end
    end
end

------------------------------------------------------------
-- SIMD/FP Data Processing
------------------------------------------------------------
function Decode.execSimdDP(cpu: CPU.CPU, insn: number, pc: integer)
    -- DUP (element): 0Q001110000imm5 000001 Rn Rd -> DUP Vd.T, Vn.Ts[index]
    if bit32.band(insn, 0xBFE0FC00) == 0x0E000400 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local imm5 = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)

        local srcLo = cpu:readVLo(rn)
        local srcHi = cpu:readVHi(rn)

        -- Extract element based on imm5
        local elem: integer
        if bit32.band(imm5, 1) == 1 then -- byte
            local idx = bit32.rshift(imm5, 1)
            local src = if idx < 8 then srcLo else srcHi
            elem = Int.band(Int.shr(src, (idx % 8) * 8), Int.MASK8)
            local lo = Int.ZERO
            for s = 0, 56, 8 do lo = Int.bor(lo, Int.shl(elem, s)) end
            cpu:writeVFull(rd, lo, if q == 1 then lo else Int.ZERO)
        elseif bit32.band(imm5, 3) == 2 then -- halfword
            local idx = bit32.rshift(imm5, 2)
            local src = if idx < 4 then srcLo else srcHi
            elem = Int.band(Int.shr(src, (idx % 4) * 16), Int.MASK16)
            local lo = Int.ZERO
            for s = 0, 48, 16 do lo = Int.bor(lo, Int.shl(elem, s)) end
            cpu:writeVFull(rd, lo, if q == 1 then lo else Int.ZERO)
        elseif bit32.band(imm5, 7) == 4 then -- word
            local idx = bit32.rshift(imm5, 3)
            local src = if idx < 2 then srcLo else srcHi
            elem = Int.band(Int.shr(src, (idx % 2) * 32), Int.MASK32)
            local lo = Int.bor(elem, Int.shl(elem, 32))
            cpu:writeVFull(rd, lo, if q == 1 then lo else Int.ZERO)
        elseif bit32.band(imm5, 15) == 8 then -- doubleword
            local idx = bit32.rshift(imm5, 4)
            elem = if idx == 0 then srcLo else srcHi
            cpu:writeVFull(rd, elem, if q == 1 then elem else Int.ZERO)
        end
        return
    end

    -- DUP (general): 0Q001110000imm5 000011 Rn Rd -> DUP Vd.T, Wn/Xn
    if bit32.band(insn, 0xBFE0FC00) == 0x0E000C00 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local imm5 = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)

        local src = cpu:readX(rn)
        local lo = Int.ZERO
        local hi = Int.ZERO

        if bit32.band(imm5, 1) == 1 then
            -- Byte: broadcast low byte
            local b = Int.band(src, Int.MASK8)
            -- Fill all bytes of lo and hi with this value
            lo = Int.ZERO
            for shift = 0, 56, 8 do
                lo = Int.bor(lo, Int.shl(b, shift))
            end
            hi = if q == 1 then lo else Int.ZERO
        elseif bit32.band(imm5, 3) == 2 then
            -- Halfword
            local h = Int.band(src, Int.MASK16)
            lo = Int.ZERO
            for s = 0, 48, 16 do lo = Int.bor(lo, Int.shl(h, s)) end
            hi = if q == 1 then lo else Int.ZERO
        elseif bit32.band(imm5, 7) == 4 then
            -- Word
            local w = Int.band(src, Int.MASK32)
            lo = Int.bor(w, Int.shl(w, 32))
            hi = if q == 1 then lo else Int.ZERO
        elseif bit32.band(imm5, 15) == 8 then
            -- Doubleword
            lo = src
            hi = if q == 1 then src else Int.ZERO
        end

        cpu:writeVFull(rd, lo, hi)
        return
    end

    -- UMOV/MOV (to general): 0Q001110000imm5 001111 Rn Rd
    if bit32.band(insn, 0xBFE0FC00) == 0x0E003C00 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local imm5 = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)

        -- Extract element
        local lo = cpu:readVLo(rn)
        local hi = cpu:readVHi(rn)

        if bit32.band(imm5, 1) == 1 then
            -- Byte: index = imm5[4:1]
            local idx = bit32.rshift(imm5, 1)
            local source = if idx < 8 then lo else hi
            local byteOff = idx % 8
            cpu:writeX(rd, Int.band(Int.shr(source, byteOff * 8), Int.MASK8))
        elseif bit32.band(imm5, 3) == 2 then
            -- Halfword
            local idx = bit32.rshift(imm5, 2)
            local source = if idx < 4 then lo else hi
            local off = (idx % 4) * 16
            cpu:writeX(rd, Int.band(Int.shr(source, off), Int.MASK16))
        elseif bit32.band(imm5, 7) == 4 then
            -- Word
            local idx = bit32.rshift(imm5, 3)
            local source = if idx < 2 then lo else hi
            local off = (idx % 2) * 32
            cpu:writeX(rd, Int.band(Int.shr(source, off), Int.MASK32))
        elseif bit32.band(imm5, 15) == 8 then
            -- Doubleword
            local idx = bit32.rshift(imm5, 4)
            local source = if idx == 0 then lo else hi
            cpu:writeX(rd, source)
        end
        return
    end

    -- MOVI/MVNI: Advanced SIMD modified immediate
    -- Encoding: 0 Q op 01111 00000 a b c cmode 01 d e f g h Rd
    -- op=0 for MOVI, op=1 for MVNI (when cmode != 111x)
    -- Detected by bits [28:19] = 0F00 (0 x 0111 1 000 00)
    -- Actually the encoding is: 0Q op 0 1111 00000 abc cmode 01 defgh Rd
    -- Let me use a broader match
    if bit32.band(insn, 0x9FF80400) == 0x0F000400 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local op = bit32.band(bit32.rshift(insn, 29), 1)
        local rd = bit32.band(insn, 0x1F)
        local abc = bit32.band(bit32.rshift(insn, 16), 0x7)
        local defgh = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local cmode = bit32.band(bit32.rshift(insn, 12), 0xF)
        local imm8 = bit32.bor(bit32.lshift(abc, 5), defgh)

        local val = Decode.decodeSIMDModifiedImm(imm8, cmode, op)
        if op == 1 and bit32.band(cmode, 0xE) ~= 0xE then
            -- MVNI: invert
            val = Int.bnot(val)
        end

        cpu:writeVFull(rd, val, if q == 1 then val else Int.ZERO)
        return
    end

    -- FMOV to/from: treat as MOV for our purposes - just handle the specific ones
    -- FMOV (general): move between FP and general register
    if bit32.band(insn, 0xFF3FFC00) == 0x1E260000 or -- FMOV Wd, Sn
       bit32.band(insn, 0xFF3FFC00) == 0x1E270000 or -- FMOV Sd, Wn
       bit32.band(insn, 0xFF3FFC00) == 0x9E260000 or -- FMOV Xd, Dn
       bit32.band(insn, 0xFF3FFC00) == 0x9E270000 or -- FMOV Dd, Xn
       bit32.band(insn, 0xFFFFFC00) == 0x9EAF0000 or -- FMOV Xd, Vn.D[1]
       bit32.band(insn, 0xFFFFFC00) == 0x9EAE0000 then -- FMOV Vd.D[1], Xn
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)
        local rmode = bit32.band(bit32.rshift(insn, 19), 0x3)
        local opcode = bit32.band(bit32.rshift(insn, 16), 0x7)
        local sf = bit32.band(bit32.rshift(insn, 31), 1)

        if opcode == 6 then -- FP -> GP
            if sf == 0 then
                cpu:writeW(rd, Int.band(cpu:readVLo(rn), Int.MASK32))
            elseif rmode == 1 then -- Vn.D[1] -> Xd
                cpu:writeX(rd, cpu:readVHi(rn))
            else
                cpu:writeX(rd, cpu:readVLo(rn))
            end
        elseif opcode == 7 then -- GP -> FP
            if sf == 0 then
                cpu:writeVFull(rd, Int.band(cpu:readX(rn), Int.MASK32), Int.ZERO)
            elseif rmode == 1 then -- Xn -> Vd.D[1]
                cpu:writeVFull(rd, cpu:readVLo(rd), cpu:readX(rn))
            else
                cpu:writeVFull(rd, cpu:readX(rn), Int.ZERO)
            end
        end
        return
    end

    -- Advanced SIMD three-same: 0 Q U 01110 size 1 Rm opcode 1 Rn Rd
    if bit32.band(insn, 0x9F200400) == 0x0E200400 then
        Decode.execSimdThreeSame(cpu, insn, pc)
        return
    end

    -- Advanced SIMD two-reg misc: 0 Q U 01110 size 10000 opcode 10 Rn Rd
    if bit32.band(insn, 0x9F3E0C00) == 0x0E200800 then
        Decode.execSimdTwoReg(cpu, insn, pc)
        return
    end

    -- Advanced SIMD across lanes: 0 Q U 01110 size 11000 opcode 10 Rn Rd
    if bit32.band(insn, 0x9F3E0C00) == 0x0E300800 then
        Decode.execSimdAcrossLanes(cpu, insn, pc)
        return
    end

    -- EXT: 0 Q 101110 000 Rm 0 imm4 0 Rn Rd
    if bit32.band(insn, 0xBFE08400) == 0x2E000000 then
        Decode.execSimdExt(cpu, insn, pc)
        return
    end

    -- Advanced SIMD shift by immediate: 0 Q U 011110 immh immb opcode 1 Rn Rd
    if bit32.band(insn, 0x9F800400) == 0x0F000400 then
        Decode.execSimdShiftImm(cpu, insn, pc)
        return
    end

    -- Advanced SIMD permude (UZP1, UZP2, ZIP1, ZIP2, TRN1, TRN2):
    if bit32.band(insn, 0xBF208C00) == 0x0E000800 then
        Decode.execSimdPermude(cpu, insn, pc)
        return
    end

    -- SIMD vector x indexed element (FMUL/FMLA/FMLS by element): top8 = 0x0F/0x2F/0x4F/0x6F
    -- Encoding: 0 Q U 01111 sz L M Rm opcode H 0 Rn Rd
    if bit32.band(insn, 0x9F000400) == 0x0F000000 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local u = bit32.band(bit32.rshift(insn, 29), 1)
        local sz = bit32.band(bit32.rshift(insn, 22), 1)
        local opcode = bit32.band(bit32.rshift(insn, 12), 0xF)
        local H = bit32.band(bit32.rshift(insn, 11), 1)
        local M = bit32.band(bit32.rshift(insn, 20), 1)
        local rm4 = bit32.band(bit32.rshift(insn, 16), 0xF)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)

        if sz == 1 then -- double
            local rm = bit32.bor(bit32.lshift(M, 4), rm4)
            local index = H
            local elemVal = if index == 0 then intToF64(cpu:readVLo(rm)) else intToF64(cpu:readVHi(rm))

            local aLo = intToF64(cpu:readVLo(rn)); local aHi = intToF64(cpu:readVHi(rn))
            if opcode == 0x9 and u == 0 then -- FMUL
                cpu:writeVFull(rd, f64ToInt(aLo * elemVal), if q == 1 then f64ToInt(aHi * elemVal) else Int.ZERO)
            elseif opcode == 0x1 and u == 0 then -- FMLA
                local dLo = intToF64(cpu:readVLo(rd)); local dHi = intToF64(cpu:readVHi(rd))
                cpu:writeVFull(rd, f64ToInt(dLo + aLo * elemVal), if q == 1 then f64ToInt(dHi + aHi * elemVal) else Int.ZERO)
            elseif opcode == 0x5 and u == 0 then -- FMLS
                local dLo = intToF64(cpu:readVLo(rd)); local dHi = intToF64(cpu:readVHi(rd))
                cpu:writeVFull(rd, f64ToInt(dLo - aLo * elemVal), if q == 1 then f64ToInt(dHi - aHi * elemVal) else Int.ZERO)
            else
                Decode.unimplemented(cpu, insn, pc)
            end
        else -- single
            local rm = rm4
            local index = bit32.bor(bit32.lshift(H, 1), M) -- for single: index = H:L (but L is at bit21)
            local L = bit32.band(bit32.rshift(insn, 21), 1)
            index = bit32.bor(bit32.lshift(H, 1), L)
            local elemBits = (index % 2) * 32
            local src = if index < 2 then cpu:readVLo(rm) else cpu:readVHi(rm)
            local elemVal = intToF32(Int.toNumber(Int.band(Int.shr(src, elemBits), Int.MASK32)))

            -- Apply to each single in rn
            local function applyS(op: (number, number) -> number)
                local srcN_lo = cpu:readVLo(rn); local srcN_hi = cpu:readVHi(rn)
                local dLo = Int.ZERO; local dHi = Int.ZERO
                for pos = 0, 32, 32 do
                    local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_lo, pos), Int.MASK32)))
                    dLo = Int.bor(dLo, Int.shl(Int.from(f32ToInt(op(a, elemVal))), pos))
                end
                if q == 1 then
                    for pos = 0, 32, 32 do
                        local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_hi, pos), Int.MASK32)))
                        dHi = Int.bor(dHi, Int.shl(Int.from(f32ToInt(op(a, elemVal))), pos))
                    end
                end
                cpu:writeVFull(rd, dLo, dHi)
            end
            if opcode == 0x9 and u == 0 then applyS(function(a,b) return a*b end)
            elseif opcode == 0x1 and u == 0 then
                -- FMLA: dst += src * elem
                applyS(function(a,b)
                    return intToF32(Int.toNumber(Int.band(Int.shr(cpu:readVLo(rd), 0), Int.MASK32))) + a*b
                end)
            else Decode.unimplemented(cpu, insn, pc) end
        end
        return
    end

    -- Scalar FP data processing (top8 = 0x1E, 0x1F only — 0x5E/0x5F/0x7E are scalar SIMD)
    local localTop8 = bit32.band(bit32.rshift(insn, 24), 0xFF)
    if localTop8 == 0x1E or localTop8 == 0x1F then
        Decode.execFPScalar(cpu, insn, pc)
        return
    end

    -- Scalar SIMD element operations (0x5E): MOV/DUP scalar from element
    if localTop8 == 0x5E then
        -- MOV (scalar): 01011110 imm5 0 00001 Rn Rd (same as DUP element, scalar form)
        if bit32.band(insn, 0xFFE0FC00) == 0x5E000400 then
            local imm5 = bit32.band(bit32.rshift(insn, 16), 0x1F)
            local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
            local rd = bit32.band(insn, 0x1F)
            local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
            local elem: integer = Int.ZERO
            if bit32.band(imm5, 1) == 1 then
                local idx = bit32.rshift(imm5, 1)
                local src = if idx < 8 then srcLo else srcHi
                elem = Int.band(Int.shr(src, (idx % 8) * 8), Int.MASK8)
            elseif bit32.band(imm5, 3) == 2 then
                local idx = bit32.rshift(imm5, 2)
                local src = if idx < 4 then srcLo else srcHi
                elem = Int.band(Int.shr(src, (idx % 4) * 16), Int.MASK16)
            elseif bit32.band(imm5, 7) == 4 then
                local idx = bit32.rshift(imm5, 3)
                local src = if idx < 2 then srcLo else srcHi
                elem = Int.band(Int.shr(src, (idx % 2) * 32), Int.MASK32)
            elseif bit32.band(imm5, 15) == 8 then
                local idx = bit32.rshift(imm5, 4)
                elem = if idx == 0 then srcLo else srcHi
            end
            cpu:writeVFull(rd, elem, Int.ZERO)
            return
        end
        Decode.unimplemented(cpu, insn, pc)
        return
    end

    -- Fallback: unimplemented
    Decode.unimplemented(cpu, insn, pc)
end

-- Helper: apply a per-element operation on vectors
function Decode.simdBinOp(cpu: CPU.CPU, insn: number, op: (integer, integer, number) -> integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local elemBits = 8 * bit32.lshift(1, size)

    local aLo = cpu:readVLo(rn); local aHi = cpu:readVHi(rn)
    local bLo = cpu:readVLo(rm); local bHi = cpu:readVHi(rm)
    local dLo = Int.ZERO; local dHi = Int.ZERO
    local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)

    local pos = 0
    while pos < 64 do
        local a = Int.band(Int.shr(aLo, pos), elemMask)
        local b = Int.band(Int.shr(bLo, pos), elemMask)
        local r = Int.band(op(a, b, elemBits), elemMask)
        dLo = Int.bor(dLo, Int.shl(r, pos))
        pos += elemBits
    end

    if q == 1 then
        pos = 0
        while pos < 64 do
            local a = Int.band(Int.shr(aHi, pos), elemMask)
            local b = Int.band(Int.shr(bHi, pos), elemMask)
            local r = Int.band(op(a, b, elemBits), elemMask)
            dHi = Int.bor(dHi, Int.shl(r, pos))
            pos += elemBits
        end
    end

    cpu:writeVFull(rd, dLo, dHi)
end

function Decode.execSimdThreeSame(cpu: CPU.CPU, insn: number, pc: integer)
    local u = bit32.band(bit32.rshift(insn, 29), 1)
    local opcode = bit32.band(bit32.rshift(insn, 11), 0x1F)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)

    -- CMEQ (register): U=1, opcode=10001 (0x11) — but actually let me check:
    -- The three-same instructions have opcode at bits[15:11]
    -- Let me decode properly:
    -- opcode is at bits [15:11] for three-same
    -- No wait: format is 0 Q U 01110 size 1 Rm opcode[4:0] 1 Rn Rd
    -- opcode is bits [15:11]
    local opcode5 = bit32.band(bit32.rshift(insn, 11), 0x1F)

    -- CMEQ (register): U=1, opcode=0x11
    if u == 1 and opcode5 == 0x11 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            return if a == b then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- CMHS (unsigned >=): U=1, opcode=0x07
    if u == 1 and opcode5 == 0x07 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            return if Int.uge(a, b) then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- CMHI (unsigned >): U=1, opcode=0x06
    if u == 1 and opcode5 == 0x06 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            return if Int.ugt(a, b) then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- CMGE (signed >=): U=0, opcode=0x07
    if u == 0 and opcode5 == 0x07 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            local as = Int.signExtend(a, bits)
            local bs = Int.signExtend(b, bits)
            return if Int.ge(as, bs) then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- CMGT (signed >): U=0, opcode=0x06
    if u == 0 and opcode5 == 0x06 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            local as = Int.signExtend(a, bits)
            local bs = Int.signExtend(b, bits)
            return if Int.gt(as, bs) then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- ADD (vector): U=0, opcode=0x10
    if u == 0 and opcode5 == 0x10 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            return Int.add(a, b)
        end)
        return
    end

    -- Logical operations: opcode=0x03, size and U determine AND/ORR/EOR/BIT/BIF/BSL
    if opcode5 == 0x03 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)
        local nLo = cpu:readVLo(rn); local nHi = cpu:readVHi(rn)
        local mLo = cpu:readVLo(rm); local mHi = cpu:readVHi(rm)

        if u == 0 and size == 0 then -- AND
            cpu:writeVFull(rd, Int.band(nLo, mLo), if q == 1 then Int.band(nHi, mHi) else Int.ZERO)
        elseif u == 0 and size == 1 then -- BIC
            cpu:writeVFull(rd, Int.band(nLo, Int.bnot(mLo)), if q == 1 then Int.band(nHi, Int.bnot(mHi)) else Int.ZERO)
        elseif u == 0 and size == 2 then -- ORR
            cpu:writeVFull(rd, Int.bor(nLo, mLo), if q == 1 then Int.bor(nHi, mHi) else Int.ZERO)
        elseif u == 0 and size == 3 then -- ORN
            cpu:writeVFull(rd, Int.bor(nLo, Int.bnot(mLo)), if q == 1 then Int.bor(nHi, Int.bnot(mHi)) else Int.ZERO)
        elseif u == 1 and size == 0 then -- EOR
            cpu:writeVFull(rd, Int.bxor(nLo, mLo), if q == 1 then Int.bxor(nHi, mHi) else Int.ZERO)
        elseif u == 1 and size == 1 then -- BSL: Vd = (Vn AND Vd) OR (Vm AND NOT Vd)
            local dLo = cpu:readVLo(rd); local dHi = cpu:readVHi(rd)
            cpu:writeVFull(rd,
                Int.bor(Int.band(nLo, dLo), Int.band(mLo, Int.bnot(dLo))),
                if q == 1 then Int.bor(Int.band(nHi, dHi), Int.band(mHi, Int.bnot(dHi))) else Int.ZERO)
        elseif u == 1 and size == 2 then -- BIT: Vd = (Vd AND NOT Vm) OR (Vn AND Vm)
            local dLo = cpu:readVLo(rd); local dHi = cpu:readVHi(rd)
            cpu:writeVFull(rd,
                Int.bor(Int.band(dLo, Int.bnot(mLo)), Int.band(nLo, mLo)),
                if q == 1 then Int.bor(Int.band(dHi, Int.bnot(mHi)), Int.band(nHi, mHi)) else Int.ZERO)
        elseif u == 1 and size == 3 then -- BIF: Vd = (Vd AND Vm) OR (Vn AND NOT Vm)
            local dLo = cpu:readVLo(rd); local dHi = cpu:readVHi(rd)
            cpu:writeVFull(rd,
                Int.bor(Int.band(dLo, mLo), Int.band(nLo, Int.bnot(mLo))),
                if q == 1 then Int.bor(Int.band(dHi, mHi), Int.band(nHi, Int.bnot(mHi))) else Int.ZERO)
        end
        return
    end

    -- ADDP (pairwise add): U=0, opcode=0x17
    if u == 0 and opcode5 == 0x17 then
        Decode.execSimdPairwise(cpu, insn, function(a, b) return Int.add(a, b) end)
        return
    end

    -- UMAXP: U=1, opcode=0x14
    if u == 1 and opcode5 == 0x14 then
        Decode.execSimdPairwise(cpu, insn, function(a, b) return if Int.uge(a, b) then a else b end)
        return
    end

    -- UMINP: U=1, opcode=0x15
    if u == 1 and opcode5 == 0x15 then
        Decode.execSimdPairwise(cpu, insn, function(a, b) return if Int.ule(a, b) then a else b end)
        return
    end

    -- CMTST: U=0, opcode=0x13
    if u == 0 and opcode5 == 0x13 then
        Decode.simdBinOp(cpu, insn, function(a, b, bits)
            return if not Int.isZero(Int.band(a, b)) then Int.sub(Int.shl(Int.ONE, bits), Int.ONE) else Int.ZERO
        end)
        return
    end

    -- SIMD FP operations: opcodes >= 0x18 with size encoding sz (bit22)
    if opcode5 >= 0x18 then
        local q = bit32.band(bit32.rshift(insn, 30), 1)
        local sz = bit32.band(bit32.rshift(insn, 22), 1) -- 0=single, 1=double
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
        local rd = bit32.band(insn, 0x1F)

        if sz == 1 then -- double precision (.2d)
            local function fpOp2d(op: (number, number) -> number)
                local aLo = intToF64(cpu:readVLo(rn)); local aHi = intToF64(cpu:readVHi(rn))
                local bLo = intToF64(cpu:readVLo(rm)); local bHi = intToF64(cpu:readVHi(rm))
                cpu:writeVFull(rd, f64ToInt(op(aLo, bLo)), if q == 1 then f64ToInt(op(aHi, bHi)) else Int.ZERO)
            end
            local bit23 = bit32.band(bit32.rshift(insn, 23), 1)
            if u == 0 and opcode5 == 0x1A and bit23 == 0 then fpOp2d(function(a,b) return a + b end); return end -- FADD
            if u == 0 and opcode5 == 0x1A and bit23 == 1 then fpOp2d(function(a,b) return a - b end); return end -- FSUB
            if u == 1 and opcode5 == 0x1B and bit23 == 0 then fpOp2d(function(a,b) return a * b end); return end -- FMUL
            if u == 1 and opcode5 == 0x1F and bit23 == 0 then fpOp2d(function(a,b) return a / b end); return end -- FDIV
            if u == 0 and opcode5 == 0x1F and bit23 == 0 then fpOp2d(function(a,b) return if a > b then a else b end); return end -- FMAX
            if u == 0 and opcode5 == 0x1F and bit23 == 1 then fpOp2d(function(a,b) return if a < b then a else b end); return end -- FMIN
            if u == 0 and opcode5 == 0x19 and bit23 == 0 then -- FMLA
                local dLo = intToF64(cpu:readVLo(rd)); local dHi = intToF64(cpu:readVHi(rd))
                local aLo = intToF64(cpu:readVLo(rn)); local aHi = intToF64(cpu:readVHi(rn))
                local bLo = intToF64(cpu:readVLo(rm)); local bHi = intToF64(cpu:readVHi(rm))
                cpu:writeVFull(rd, f64ToInt(dLo + aLo*bLo), if q == 1 then f64ToInt(dHi + aHi*bHi) else Int.ZERO)
                return
            end
            if u == 0 and opcode5 == 0x19 and bit23 == 1 then -- FMLS
                local dLo = intToF64(cpu:readVLo(rd)); local dHi = intToF64(cpu:readVHi(rd))
                local aLo = intToF64(cpu:readVLo(rn)); local aHi = intToF64(cpu:readVHi(rn))
                local bLo = intToF64(cpu:readVLo(rm)); local bHi = intToF64(cpu:readVHi(rm))
                cpu:writeVFull(rd, f64ToInt(dLo - aLo*bLo), if q == 1 then f64ToInt(dHi - aHi*bHi) else Int.ZERO)
                return
            end
        elseif sz == 0 then -- single precision (.4s or .2s)
            local function fpOp4s(op: (number, number) -> number)
                local srcN_lo = cpu:readVLo(rn); local srcN_hi = cpu:readVHi(rn)
                local srcM_lo = cpu:readVLo(rm); local srcM_hi = cpu:readVHi(rm)
                local dLo = Int.ZERO; local dHi = Int.ZERO
                for pos = 0, 32, 32 do
                    local aN = intToF32(Int.toNumber(Int.band(Int.shr(srcN_lo, pos), Int.MASK32)))
                    local bN = intToF32(Int.toNumber(Int.band(Int.shr(srcM_lo, pos), Int.MASK32)))
                    dLo = Int.bor(dLo, Int.shl(Int.from(f32ToInt(op(aN, bN))), pos))
                end
                if q == 1 then
                    for pos = 0, 32, 32 do
                        local aN = intToF32(Int.toNumber(Int.band(Int.shr(srcN_hi, pos), Int.MASK32)))
                        local bN = intToF32(Int.toNumber(Int.band(Int.shr(srcM_hi, pos), Int.MASK32)))
                        dHi = Int.bor(dHi, Int.shl(Int.from(f32ToInt(op(aN, bN))), pos))
                    end
                end
                cpu:writeVFull(rd, dLo, dHi)
            end
            local bit23s = bit32.band(bit32.rshift(insn, 23), 1)
            if u == 0 and opcode5 == 0x1A and bit23s == 0 then fpOp4s(function(a,b) return a + b end); return end
            if u == 0 and opcode5 == 0x1A and bit23s == 1 then fpOp4s(function(a,b) return a - b end); return end
            if u == 1 and opcode5 == 0x1B and bit23s == 0 then fpOp4s(function(a,b) return a * b end); return end
            if u == 1 and opcode5 == 0x1F and bit23s == 0 then fpOp4s(function(a,b) return a / b end); return end
            if u == 0 and opcode5 == 0x1F and bit23s == 0 then fpOp4s(function(a,b) return if a > b then a else b end); return end
            if u == 0 and opcode5 == 0x1F and bit23s == 1 then fpOp4s(function(a,b) return if a < b then a else b end); return end
            if u == 0 and opcode5 == 0x19 and bit23s == 0 then -- FMLA .4s/.2s
                local srcN_lo = cpu:readVLo(rn); local srcN_hi = cpu:readVHi(rn)
                local srcM_lo = cpu:readVLo(rm); local srcM_hi = cpu:readVHi(rm)
                local dLo = cpu:readVLo(rd); local dHi = cpu:readVHi(rd)
                local rLo = Int.ZERO; local rHi = Int.ZERO
                for pos = 0, 32, 32 do
                    local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_lo, pos), Int.MASK32)))
                    local b = intToF32(Int.toNumber(Int.band(Int.shr(srcM_lo, pos), Int.MASK32)))
                    local d = intToF32(Int.toNumber(Int.band(Int.shr(dLo, pos), Int.MASK32)))
                    rLo = Int.bor(rLo, Int.shl(Int.from(f32ToInt(d + a * b)), pos))
                end
                if q == 1 then
                    for pos = 0, 32, 32 do
                        local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_hi, pos), Int.MASK32)))
                        local b = intToF32(Int.toNumber(Int.band(Int.shr(srcM_hi, pos), Int.MASK32)))
                        local d = intToF32(Int.toNumber(Int.band(Int.shr(dHi, pos), Int.MASK32)))
                        rHi = Int.bor(rHi, Int.shl(Int.from(f32ToInt(d + a * b)), pos))
                    end
                end
                cpu:writeVFull(rd, rLo, rHi); return
            end
            if u == 0 and opcode5 == 0x19 and bit23s == 1 then -- FMLS .4s/.2s
                local srcN_lo = cpu:readVLo(rn); local srcN_hi = cpu:readVHi(rn)
                local srcM_lo = cpu:readVLo(rm); local srcM_hi = cpu:readVHi(rm)
                local dLo = cpu:readVLo(rd); local dHi = cpu:readVHi(rd)
                local rLo = Int.ZERO; local rHi = Int.ZERO
                for pos = 0, 32, 32 do
                    local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_lo, pos), Int.MASK32)))
                    local b = intToF32(Int.toNumber(Int.band(Int.shr(srcM_lo, pos), Int.MASK32)))
                    local d = intToF32(Int.toNumber(Int.band(Int.shr(dLo, pos), Int.MASK32)))
                    rLo = Int.bor(rLo, Int.shl(Int.from(f32ToInt(d - a * b)), pos))
                end
                if q == 1 then
                    for pos = 0, 32, 32 do
                        local a = intToF32(Int.toNumber(Int.band(Int.shr(srcN_hi, pos), Int.MASK32)))
                        local b = intToF32(Int.toNumber(Int.band(Int.shr(srcM_hi, pos), Int.MASK32)))
                        local d = intToF32(Int.toNumber(Int.band(Int.shr(dHi, pos), Int.MASK32)))
                        rHi = Int.bor(rHi, Int.shl(Int.from(f32ToInt(d - a * b)), pos))
                    end
                end
                cpu:writeVFull(rd, rLo, rHi); return
            end
        end
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSimdAddp(cpu: CPU.CPU, insn: number)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)
    Decode.execSimdPairwise(cpu, insn, function(a, b) return Int.add(a, b) end)
end

function Decode.execSimdPairwise(cpu: CPU.CPU, insn: number, op: (integer, integer) -> integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)
    local elemBits = 8 * bit32.lshift(1, size)
    local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)

    -- Concatenate Rn and Rm elements, then pairwise reduce
    local elements: { integer } = {}
    -- Extract all elements from Rn then Rm
    local function extract(lo: integer, hi: integer)
        local pos = 0
        while pos < 64 do
            table.insert(elements, Int.band(Int.shr(lo, pos), elemMask))
            pos += elemBits
        end
        if q == 1 then
            pos = 0
            while pos < 64 do
                table.insert(elements, Int.band(Int.shr(hi, pos), elemMask))
                pos += elemBits
            end
        end
    end
    extract(cpu:readVLo(rn), cpu:readVHi(rn))
    extract(cpu:readVLo(rm), cpu:readVHi(rm))

    -- Pairwise reduce: take pairs and apply op
    local results: { integer } = {}
    for idx = 1, #elements, 2 do
        table.insert(results, Int.band(op(elements[idx], elements[idx + 1]), elemMask))
    end

    -- Pack results back into Rd
    local dLo = Int.ZERO; local dHi = Int.ZERO
    local pos = 0; local idx = 1
    while pos < 64 and idx <= #results do
        dLo = Int.bor(dLo, Int.shl(results[idx], pos))
        pos += elemBits; idx += 1
    end
    if q == 1 then
        pos = 0
        while pos < 64 and idx <= #results do
            dHi = Int.bor(dHi, Int.shl(results[idx], pos))
            pos += elemBits; idx += 1
        end
    end
    cpu:writeVFull(rd, dLo, dHi)
end

function Decode.execSimdTwoReg(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local u = bit32.band(bit32.rshift(insn, 29), 1)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local opcode = bit32.band(bit32.rshift(insn, 12), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    -- CMEQ #0: U=0, opcode=01001
    if u == 0 and opcode == 0x09 then
        local elemBits = 8 * bit32.lshift(1, size)
        local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
        local allOnes = elemMask
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local dLo = Int.ZERO; local dHi = Int.ZERO
        local pos = 0
        while pos < 64 do
            local e = Int.band(Int.shr(srcLo, pos), elemMask)
            if Int.isZero(e) then dLo = Int.bor(dLo, Int.shl(allOnes, pos)) end
            pos += elemBits
        end
        if q == 1 then
            pos = 0
            while pos < 64 do
                local e = Int.band(Int.shr(srcHi, pos), elemMask)
                if Int.isZero(e) then dHi = Int.bor(dHi, Int.shl(allOnes, pos)) end
                pos += elemBits
            end
        end
        cpu:writeVFull(rd, dLo, dHi)
        return
    end

    -- NOT: U=1, opcode=00101, size=00
    if u == 1 and opcode == 0x05 and size == 0 then
        local lo = Int.bnot(cpu:readVLo(rn))
        local hi = if q == 1 then Int.bnot(cpu:readVHi(rn)) else Int.ZERO
        cpu:writeVFull(rd, lo, hi)
        return
    end

    -- CNT: U=0, opcode=00101, size=00
    if u == 0 and opcode == 0x05 and size == 0 then
        -- Population count per byte
        local function popcnt8(v: integer): integer
            local result = Int.ZERO
            for byteIdx = 0, 7 do
                local b = Int.toNumber(Int.band(Int.shr(v, byteIdx * 8), Int.MASK8))
                local cnt = 0
                while b > 0 do cnt += bit32.band(b, 1); b = bit32.rshift(b, 1) end
                result = Int.bor(result, Int.shl(Int.from(cnt), byteIdx * 8))
            end
            return result
        end
        local lo = popcnt8(cpu:readVLo(rn))
        local hi = if q == 1 then popcnt8(cpu:readVHi(rn)) else Int.ZERO
        cpu:writeVFull(rd, lo, hi)
        return
    end

    -- REV64: U=0, opcode=00000
    if u == 0 and opcode == 0x00 then
        -- Reverse elements within each 64-bit doubleword
        local elemBits = 8 * bit32.lshift(1, size)
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local function rev64Elem(src: integer): integer
            local result = Int.ZERO
            local numElems = math.floor(64 / elemBits)
            local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
            for idx = 0, numElems - 1 do
                local e = Int.band(Int.shr(src, idx * elemBits), elemMask)
                result = Int.bor(result, Int.shl(e, (numElems - 1 - idx) * elemBits))
            end
            return result
        end
        local dLo = rev64Elem(srcLo)
        local dHi = if q == 1 then rev64Elem(srcHi) else Int.ZERO
        cpu:writeVFull(rd, dLo, dHi)
        return
    end

    -- XTN (narrow): U=0, opcode=10010
    if u == 0 and opcode == 0x12 then
        -- Narrow each element to half width
        local elemBits = 8 * bit32.lshift(1, size) -- source element size
        local narrowBits = elemBits -- result is half the next-size up? Actually XTN narrows from 2*size to size
        -- XTN: extract lower half of each double-width element
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local dstElemBits = elemBits
        local srcElemBits = elemBits * 2
        local elemMask = Int.sub(Int.shl(Int.ONE, dstElemBits), Int.ONE)
        local srcMask = Int.sub(Int.shl(Int.ONE, srcElemBits), Int.ONE)
        local result = Int.ZERO
        local pos = 0; local srcPos = 0
        -- Source is in rn (128-bit), extract lower halves
        local function getElem(idx: number): integer
            local bitPos = idx * srcElemBits
            local src = if bitPos < 64 then srcLo else srcHi
            local localPos = bitPos % 64
            return Int.band(Int.shr(src, localPos), srcMask)
        end
        local numElems = math.floor((if q == 1 then 128 else 64) / srcElemBits)
        for idx = 0, numElems - 1 do
            local e = Int.band(getElem(idx), elemMask)
            result = Int.bor(result, Int.shl(e, idx * dstElemBits))
        end
        -- If Q=0, result goes to lower half of Vd. If Q=1, upper half (XTN2)
        if q == 0 then
            cpu:writeVFull(rd, result, Int.ZERO)
        else
            cpu:writeVFull(rd, cpu:readVLo(rd), result)
        end
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSimdAcrossLanes(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local u = bit32.band(bit32.rshift(insn, 29), 1)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local opcode = bit32.band(bit32.rshift(insn, 12), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    -- ADDV: U=0, opcode=11011
    if opcode == 0x1B then
        local elemBits = 8 * bit32.lshift(1, size)
        local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local sum = Int.ZERO
        local pos = 0
        while pos < 64 do
            sum = Int.add(sum, Int.band(Int.shr(srcLo, pos), elemMask))
            pos += elemBits
        end
        if q == 1 then
            pos = 0
            while pos < 64 do
                sum = Int.add(sum, Int.band(Int.shr(srcHi, pos), elemMask))
                pos += elemBits
            end
        end
        cpu:writeVFull(rd, Int.band(sum, elemMask), Int.ZERO)
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSimdExt(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local imm4 = bit32.band(bit32.rshift(insn, 11), 0xF)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    -- EXT: extract imm4 bytes from Vm:Vn concatenation
    local nLo = cpu:readVLo(rn); local nHi = cpu:readVHi(rn)
    local mLo = cpu:readVLo(rm); local mHi = cpu:readVHi(rm)

    -- Build byte array: [Vn bytes] [Vm bytes]
    local totalBytes = if q == 1 then 32 else 16
    local function getByte(idx: number): number
        local src: integer
        if idx < 8 then src = nLo
        elseif idx < 16 then src = nHi; idx -= 8
        elseif idx < 24 then src = mLo; idx -= 16
        else src = mHi; idx -= 24 end
        return Int.toNumber(Int.band(Int.shr(src, idx * 8), Int.MASK8))
    end

    local dLo = Int.ZERO; local dHi = Int.ZERO
    local vecBytes = if q == 1 then 16 else 8
    for byteIdx = 0, vecBytes - 1 do
        local b = getByte(imm4 + byteIdx)
        if byteIdx < 8 then
            dLo = Int.bor(dLo, Int.shl(Int.from(b), byteIdx * 8))
        else
            dHi = Int.bor(dHi, Int.shl(Int.from(b), (byteIdx - 8) * 8))
        end
    end
    cpu:writeVFull(rd, dLo, dHi)
end

function Decode.execSimdShiftImm(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local u = bit32.band(bit32.rshift(insn, 29), 1)
    local immh = bit32.band(bit32.rshift(insn, 19), 0xF)
    local immb = bit32.band(bit32.rshift(insn, 16), 0x7)
    local opcode = bit32.band(bit32.rshift(insn, 11), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    -- Determine element size from immh
    local elemBits: number
    if bit32.band(immh, 8) ~= 0 then elemBits = 64
    elseif bit32.band(immh, 4) ~= 0 then elemBits = 32
    elseif bit32.band(immh, 2) ~= 0 then elemBits = 16
    else elemBits = 8 end

    local immhb = bit32.bor(bit32.lshift(immh, 3), immb)

    -- SHL: U=0, opcode=01010
    if u == 0 and opcode == 0x0A then
        local shift = immhb - elemBits
        local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local dLo = Int.ZERO; local dHi = Int.ZERO
        local pos = 0
        while pos < 64 do
            local e = Int.band(Int.shr(srcLo, pos), elemMask)
            dLo = Int.bor(dLo, Int.shl(Int.band(Int.shl(e, shift), elemMask), pos))
            pos += elemBits
        end
        if q == 1 then
            pos = 0
            while pos < 64 do
                local e = Int.band(Int.shr(srcHi, pos), elemMask)
                dHi = Int.bor(dHi, Int.shl(Int.band(Int.shl(e, shift), elemMask), pos))
                pos += elemBits
            end
        end
        cpu:writeVFull(rd, dLo, dHi)
        return
    end

    -- USHR: U=1, opcode=00000
    if u == 1 and opcode == 0x00 then
        local shift = 2 * elemBits - immhb
        local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local dLo = Int.ZERO; local dHi = Int.ZERO
        local pos = 0
        while pos < 64 do
            local e = Int.band(Int.shr(srcLo, pos), elemMask)
            dLo = Int.bor(dLo, Int.shl(Int.shr(e, shift), pos))
            pos += elemBits
        end
        if q == 1 then
            pos = 0
            while pos < 64 do
                local e = Int.band(Int.shr(srcHi, pos), elemMask)
                dHi = Int.bor(dHi, Int.shl(Int.shr(e, shift), pos))
                pos += elemBits
            end
        end
        cpu:writeVFull(rd, dLo, dHi)
        return
    end

    -- SHRN/SHRN2: U=0, opcode=10000
    if u == 0 and opcode == 0x10 then
        -- Shift right narrow: reads FULL 128-bit source, narrows to 64-bit result
        local shift = 2 * elemBits - immhb
        local wideElemBits = elemBits * 2
        local wideMask = Int.sub(Int.shl(Int.ONE, wideElemBits), Int.ONE)
        local narrowMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)
        local srcLo = cpu:readVLo(rn); local srcHi = cpu:readVHi(rn)
        local result = Int.ZERO
        local pos = 0; local dstPos = 0
        -- Process elements from lower 64 bits of source
        while pos < 64 do
            local e = Int.band(Int.shr(srcLo, pos), wideMask)
            local narrowed = Int.band(Int.shr(e, shift), narrowMask)
            result = Int.bor(result, Int.shl(narrowed, dstPos))
            pos += wideElemBits; dstPos += elemBits
        end
        -- Process elements from upper 64 bits of source
        pos = 0
        while pos < 64 do
            local e = Int.band(Int.shr(srcHi, pos), wideMask)
            local narrowed = Int.band(Int.shr(e, shift), narrowMask)
            result = Int.bor(result, Int.shl(narrowed, dstPos))
            pos += wideElemBits; dstPos += elemBits
        end
        -- Q=0: write to lower half of Vd. Q=1: write to upper half (SHRN2)
        if q == 0 then
            cpu:writeVFull(rd, result, Int.ZERO)
        else
            cpu:writeVFull(rd, cpu:readVLo(rd), result)
        end
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSimdPermude(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local size = bit32.band(bit32.rshift(insn, 22), 0x3)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local opcode = bit32.band(bit32.rshift(insn, 12), 0x7)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local elemBits = 8 * bit32.lshift(1, size)
    local elemMask = Int.sub(Int.shl(Int.ONE, elemBits), Int.ONE)

    local function getElem(reg: number, idx: number): integer
        local bitPos = idx * elemBits
        local src = if bitPos < 64 then cpu:readVLo(reg) else cpu:readVHi(reg)
        local localPos = bitPos % 64
        return Int.band(Int.shr(src, localPos), elemMask)
    end

    local numElems = (if q == 1 then 128 else 64) / elemBits

    if opcode == 1 then -- UZP1
        -- Interleave even elements: [Rn[0], Rn[2], ..., Rm[0], Rm[2], ...]
        local results: { integer } = {}
        for idx = 0, numElems - 1, 2 do table.insert(results, getElem(rn, idx)) end
        for idx = 0, numElems - 1, 2 do table.insert(results, getElem(rm, idx)) end
        local dLo = Int.ZERO; local dHi = Int.ZERO
        for idx, v in results do
            local bitPos = (idx - 1) * elemBits
            if bitPos < 64 then
                dLo = Int.bor(dLo, Int.shl(v, bitPos))
            else
                dHi = Int.bor(dHi, Int.shl(v, bitPos - 64))
            end
        end
        cpu:writeVFull(rd, dLo, dHi)
        return
    end

    Decode.unimplemented(cpu, insn, pc)
end

------------------------------------------------------------
-- SIMD/FP Load/Store
------------------------------------------------------------

function Decode.execLoadStorePairSimd(cpu: CPU.CPU, insn: number, pc: integer)
    local opc = bit32.band(bit32.rshift(insn, 30), 0x3)
    local isLoad = bit32.band(bit32.rshift(insn, 22), 1) == 1
    local imm7 = bit32.band(bit32.rshift(insn, 15), 0x7F)
    local rt2 = bit32.band(bit32.rshift(insn, 10), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    -- opc: 00=S(32-bit), 01=D(64-bit), 10=Q(128-bit)
    local dataSize = if opc == 0 then 4 elseif opc == 1 then 8 else 16
    local offset = signExtendN(imm7, 7) * dataSize

    local base = cpu:readXOrSP(rn)
    local mode = bit32.band(bit32.rshift(insn, 23), 0x7)
    local addr: integer
    local doWriteback = false

    if mode == 1 then -- post-index
        addr = base
        doWriteback = true
    elseif mode == 3 then -- pre-index
        addr = Int.add(base, Int.from(offset))
        doWriteback = true
    else -- signed offset (mode 2) or no-alloc (mode 0)
        addr = Int.add(base, Int.from(offset))
    end

    if isLoad then
        if dataSize == 4 then
            local v1 = Int.from(cpu.mem:readU32(addr))
            local v2 = Int.from(cpu.mem:readU32(Int.add(addr, Int.from(4))))
            cpu:writeVLo(rt, v1)
            cpu:writeVLo(rt2, v2)
        elseif dataSize == 8 then
            local v1 = cpu.mem:readU64(addr)
            local v2 = cpu.mem:readU64(Int.add(addr, Int.from(8)))
            cpu:writeVLo(rt, v1)
            cpu:writeVLo(rt2, v2)
        else -- 16
            local lo1 = cpu.mem:readU64(addr)
            local hi1 = cpu.mem:readU64(Int.add(addr, Int.from(8)))
            local lo2 = cpu.mem:readU64(Int.add(addr, Int.from(16)))
            local hi2 = cpu.mem:readU64(Int.add(addr, Int.from(24)))
            cpu:writeVFull(rt, lo1, hi1)
            cpu:writeVFull(rt2, lo2, hi2)
        end
    else
        if dataSize == 4 then
            cpu.mem:writeU32(addr, Int.toNumber(Int.band(cpu:readVLo(rt), Int.MASK32)))
            cpu.mem:writeU32(Int.add(addr, Int.from(4)), Int.toNumber(Int.band(cpu:readVLo(rt2), Int.MASK32)))
        elseif dataSize == 8 then
            cpu.mem:writeU64(addr, cpu:readVLo(rt))
            cpu.mem:writeU64(Int.add(addr, Int.from(8)), cpu:readVLo(rt2))
        else -- 16
            cpu.mem:writeU64(addr, cpu:readVLo(rt))
            cpu.mem:writeU64(Int.add(addr, Int.from(8)), cpu:readVHi(rt))
            cpu.mem:writeU64(Int.add(addr, Int.from(16)), cpu:readVLo(rt2))
            cpu.mem:writeU64(Int.add(addr, Int.from(24)), cpu:readVHi(rt2))
        end
    end

    if doWriteback then
        local wback = if mode == 1 then Int.add(base, Int.from(offset)) else Int.add(base, Int.from(offset))
        cpu:writeXOrSP(rn, wback)
    end
end

function Decode.execLoadStoreRegSimd(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3)
    local opc = bit32.band(bit32.rshift(insn, 22), 0x3)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)
    local op4 = bit32.band(bit32.rshift(insn, 10), 0x3)
    local op3 = bit32.band(bit32.rshift(insn, 21), 1)

    -- For SIMD, size and opc together determine the access size:
    -- size=00,opc=00 -> STR B (8-bit)
    -- size=01,opc=00 -> STR H (16-bit)
    -- size=10,opc=00 -> STR S (32-bit)
    -- size=11,opc=00 -> STR D (64-bit)
    -- size=00,opc=10 -> STR Q (128-bit)
    local dataSize: number
    local isLoad: boolean
    if opc == 0 then
        isLoad = false
        dataSize = bit32.lshift(1, size) -- 1,2,4,8
    elseif opc == 1 then
        isLoad = true
        dataSize = bit32.lshift(1, size)
    elseif opc == 2 then
        isLoad = false
        dataSize = 16 -- Q store (size must be 0)
    else
        isLoad = true
        dataSize = 16 -- Q load
    end

    local base = cpu:readXOrSP(rn)
    local addr: integer
    local doWriteback = false
    local wbackAddr = base

    if op3 == 1 and op4 == 2 then
        -- Register offset
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local option = bit32.band(bit32.rshift(insn, 13), 0x7)
        local s = bit32.band(bit32.rshift(insn, 12), 1)
        local scale = if dataSize == 16 then 4 elseif dataSize == 8 then 3 elseif dataSize == 4 then 2 elseif dataSize == 2 then 1 else 0
        local shift = if s == 1 then scale else 0
        local offset = applyExtend(cpu:readX(rm), option, shift)
        addr = Int.add(base, offset)
    elseif op4 == 0 then
        -- Unscaled immediate
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        addr = Int.add(base, signExtendImm(imm9, 9))
    elseif op4 == 1 then
        -- Post-index
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        addr = base
        wbackAddr = Int.add(base, signExtendImm(imm9, 9))
        doWriteback = true
    elseif op4 == 3 then
        -- Pre-index
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        addr = Int.add(base, signExtendImm(imm9, 9))
        wbackAddr = addr
        doWriteback = true
    else
        local imm9 = bit32.band(bit32.rshift(insn, 12), 0x1FF)
        addr = Int.add(base, signExtendImm(imm9, 9))
    end

    Decode.doSimdLoadStore(cpu, rt, addr, dataSize, isLoad)

    if doWriteback then
        cpu:writeXOrSP(rn, wbackAddr)
    end
end

function Decode.execLoadStoreUnsignedOffSimd(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3)
    local opc = bit32.band(bit32.rshift(insn, 22), 0x3)
    local imm12 = bit32.band(bit32.rshift(insn, 10), 0xFFF)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local dataSize: number
    local isLoad: boolean
    if opc == 0 then
        isLoad = false
        dataSize = bit32.lshift(1, size)
    elseif opc == 1 then
        isLoad = true
        dataSize = bit32.lshift(1, size)
    elseif opc == 2 then
        isLoad = false
        dataSize = 16
    else
        isLoad = true
        dataSize = 16
    end

    local scale = if dataSize == 16 then 4 elseif dataSize == 8 then 3 elseif dataSize == 4 then 2 elseif dataSize == 2 then 1 else 0
    local offset = imm12 * bit32.lshift(1, scale)
    local base = cpu:readXOrSP(rn)
    local addr = Int.add(base, Int.from(offset))

    Decode.doSimdLoadStore(cpu, rt, addr, dataSize, isLoad)
end

------------------------------------------------------------
-- Scalar Floating-Point operations
------------------------------------------------------------

function Decode.execFPScalar(cpu: CPU.CPU, insn: number, pc: integer)
    local top8 = bit32.band(bit32.rshift(insn, 24), 0xFF)
    local ftype = bit32.band(bit32.rshift(insn, 22), 0x3) -- 0=single, 1=double
    local rd = bit32.band(insn, 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)

    -- FP data processing 2 source: x0011110 ftype 1 Rm opcode 10 Rn Rd
    if bit32.band(insn, 0xFF200C00) == 0x1E200800 then
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local opcode = bit32.band(bit32.rshift(insn, 12), 0xF)
        if ftype == 1 then -- double
            local a = readFPD(cpu, rn)
            local b = readFPD(cpu, rm)
            local result: number
            if opcode == 0 then result = a * b      -- FMUL
            elseif opcode == 1 then result = a / b  -- FDIV
            elseif opcode == 2 then result = a + b  -- FADD
            elseif opcode == 3 then result = a - b  -- FSUB
            elseif opcode == 4 then result = b      -- FMAX (simplified)
                result = if a > b then a else b
            elseif opcode == 5 then                 -- FMIN
                result = if a < b then a else b
            elseif opcode == 6 then result = if a > b then a else b -- FMAXNM
            elseif opcode == 7 then result = if a < b then a else b -- FMINNM
            elseif opcode == 8 then                 -- FNMUL
                result = -(a * b)
            else
                Decode.unimplemented(cpu, insn, pc); return
            end
            writeFPD(cpu, rd, result)
        elseif ftype == 0 then -- single
            local a = readFPS(cpu, rn)
            local b = readFPS(cpu, rm)
            local result: number
            if opcode == 0 then result = a * b
            elseif opcode == 1 then result = a / b
            elseif opcode == 2 then result = a + b
            elseif opcode == 3 then result = a - b
            elseif opcode == 4 then result = if a > b then a else b
            elseif opcode == 5 then result = if a < b then a else b
            elseif opcode == 6 then result = if a > b then a else b
            elseif opcode == 7 then result = if a < b then a else b
            elseif opcode == 8 then result = -(a * b)
            else
                Decode.unimplemented(cpu, insn, pc); return
            end
            writeFPS(cpu, rd, result)
        else
            Decode.unimplemented(cpu, insn, pc)
        end
        return
    end

    -- FP data processing 1 source: x0011110 ftype 1 opcode[5:0] 10000 Rn Rd
    if bit32.band(insn, 0xFF207C00) == 0x1E204000 then
        local opcode = bit32.band(bit32.rshift(insn, 15), 0x3F)
        if ftype == 1 then -- double
            local a = readFPD(cpu, rn)
            if opcode == 0 then writeFPD(cpu, rd, a)               -- FMOV
            elseif opcode == 1 then writeFPD(cpu, rd, math.abs(a)) -- FABS
            elseif opcode == 2 then writeFPD(cpu, rd, -a)          -- FNEG
            elseif opcode == 3 then writeFPD(cpu, rd, math.sqrt(a))-- FSQRT
            elseif opcode == 4 then                                -- FCVT D->S
                writeFPS(cpu, rd, a)
            elseif opcode == 8 then writeFPD(cpu, rd, if a >= 0 then math.floor(a + 0.5) else math.ceil(a - 0.5)) -- FRINTN
            elseif opcode == 9 then writeFPD(cpu, rd, if a >= 0 then math.floor(a + 0.5) else math.ceil(a - 0.5)) -- FRINTP
            elseif opcode == 10 then writeFPD(cpu, rd, if a >= 0 then math.floor(a) else math.ceil(a))            -- FRINTM
            elseif opcode == 11 then writeFPD(cpu, rd, if a >= 0 then math.floor(a) else math.ceil(a))            -- FRINTZ (toward zero)
                writeFPD(cpu, rd, if a >= 0 then math.floor(a) else math.ceil(a))
            else
                Decode.unimplemented(cpu, insn, pc)
            end
        elseif ftype == 0 then -- single
            local a = readFPS(cpu, rn)
            if opcode == 0 then writeFPS(cpu, rd, a)
            elseif opcode == 1 then writeFPS(cpu, rd, math.abs(a))
            elseif opcode == 2 then writeFPS(cpu, rd, -a)
            elseif opcode == 3 then writeFPS(cpu, rd, math.sqrt(a))
            elseif opcode == 5 then writeFPD(cpu, rd, a)           -- FCVT S->D
            else
                Decode.unimplemented(cpu, insn, pc)
            end
        else
            Decode.unimplemented(cpu, insn, pc)
        end
        return
    end

    -- FP compare: x0011110 ftype 1 Rm 00 1000 Rn opcode
    if bit32.band(insn, 0xFF20FC07) == 0x1E202000 then
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local opc = bit32.band(bit32.rshift(insn, 3), 0x3)
        local a: number
        local b: number
        if ftype == 1 then
            a = readFPD(cpu, rn)
            b = if bit32.band(opc, 1) == 1 then 0.0 else readFPD(cpu, rm)
        else
            a = readFPS(cpu, rn)
            b = if bit32.band(opc, 1) == 1 then 0.0 else readFPS(cpu, rm)
        end
        if a ~= a or b ~= b then -- NaN
            cpu.N = false; cpu.Z = false; cpu.C = true; cpu.V = true
        elseif a == b then
            cpu.N = false; cpu.Z = true; cpu.C = true; cpu.V = false
        elseif a < b then
            cpu.N = true; cpu.Z = false; cpu.C = false; cpu.V = false
        else -- a > b
            cpu.N = false; cpu.Z = false; cpu.C = true; cpu.V = false
        end
        return
    end

    -- FP conditional select: x0011110 ftype 1 Rm cond 11 Rn Rd
    if bit32.band(insn, 0xFF200C00) == 0x1E200C00 then
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local cond = bit32.band(bit32.rshift(insn, 12), 0xF)
        if cpu:evalCondition(cond) then
            cpu:writeVFull(rd, cpu:readVLo(rn), Int.ZERO)
        else
            cpu:writeVFull(rd, cpu:readVLo(rm), Int.ZERO)
        end
        return
    end

    -- FP data processing 3 source: x0011111 ftype o1 Rm o0 Ra Rn Rd
    -- FMADD, FMSUB, FNMADD, FNMSUB
    if bit32.band(top8, 0xBF) == 0x1F then
        local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
        local o1 = bit32.band(bit32.rshift(insn, 21), 1)
        local o0 = bit32.band(bit32.rshift(insn, 15), 1)
        local ra = bit32.band(bit32.rshift(insn, 10), 0x1F)
        if ftype == 1 then -- double
            local a = readFPD(cpu, rn)
            local b = readFPD(cpu, rm)
            local c = readFPD(cpu, ra)
            local result: number
            if o1 == 0 and o0 == 0 then result = a * b + c      -- FMADD
            elseif o1 == 0 and o0 == 1 then result = -(a * b) + c -- FMSUB (= c - a*b)
            elseif o1 == 1 and o0 == 0 then result = -(a * b + c) -- FNMADD
            else result = a * b - c                               -- FNMSUB
            end
            writeFPD(cpu, rd, result)
        elseif ftype == 0 then -- single
            local a = readFPS(cpu, rn)
            local b = readFPS(cpu, rm)
            local c = readFPS(cpu, ra)
            local result: number
            if o1 == 0 and o0 == 0 then result = a * b + c
            elseif o1 == 0 and o0 == 1 then result = -(a * b) + c
            elseif o1 == 1 and o0 == 0 then result = -(a * b + c)
            else result = a * b - c
            end
            writeFPS(cpu, rd, result)
        else
            Decode.unimplemented(cpu, insn, pc)
        end
        return
    end

    -- FP immediate: x0011110 ftype 1 imm8 100 imm5 Rd
    if bit32.band(insn, 0xFF201C00) == 0x1E201000 then
        local imm8 = bit32.band(bit32.rshift(insn, 13), 0xFF)
        -- Decode FP immediate: sign(1) exp(3, biased) frac(4)
        local sign = bit32.band(bit32.rshift(imm8, 7), 1)
        local exp = bit32.band(bit32.rshift(imm8, 4), 0x7)
        local frac = bit32.band(imm8, 0xF)
        if ftype == 1 then -- double
            -- exp biased by (1023 - 3) for the 3-bit exp field: actual_exp = exp XOR 0x4 + 1020
            local biasedExp = bit32.bxor(exp, 0x4) + 1020
            local bits = Int.ZERO
            bits = Int.bor(bits, Int.shl(Int.from(sign), 63))
            bits = Int.bor(bits, Int.shl(Int.from(biasedExp), 52))
            bits = Int.bor(bits, Int.shl(Int.from(frac), 48))
            cpu:writeVFull(rd, bits, Int.ZERO)
        else -- single
            local biasedExp = bit32.bxor(exp, 0x4) + 124
            local bits = bit32.bor(bit32.bor(bit32.lshift(sign, 31), bit32.lshift(biasedExp, 23)), bit32.lshift(frac, 19))
            cpu:writeVFull(rd, Int.from(bits), Int.ZERO)
        end
        return
    end

    -- FP<->integer conversions: x0011110 ftype 1 rmode opcode 000000 Rn Rd
    if bit32.band(insn, 0xFF20FC00) == 0x1E200000 then
        local rmode = bit32.band(bit32.rshift(insn, 19), 0x3)
        local opcode = bit32.band(bit32.rshift(insn, 16), 0x7)
        local sf = bit32.band(bit32.rshift(insn, 31), 1)

        if opcode == 6 then -- FMOV FP->GP (handled earlier in the FMOV general section)
            -- Already handled by the FMOV block
            if sf == 0 then
                cpu:writeW(rd, Int.band(cpu:readVLo(rn), Int.MASK32))
            else
                cpu:writeX(rd, cpu:readVLo(rn))
            end
            return
        elseif opcode == 7 then -- FMOV GP->FP
            if sf == 0 then
                cpu:writeVFull(rd, Int.band(cpu:readX(rn), Int.MASK32), Int.ZERO)
            else
                cpu:writeVFull(rd, cpu:readX(rn), Int.ZERO)
            end
            return
        end

        -- FCVT (int->fp, fp->int)
        if ftype == 1 then -- source is double
            local fpVal = readFPD(cpu, rn)
            if opcode == 0 then -- FCVT*S (fp->signed int)
                local intVal: number
                if rmode == 0 then intVal = math.round(fpVal)       -- FCVTNS
                elseif rmode == 1 then                              -- FCVTPS
                    intVal = math.ceil(fpVal)
                elseif rmode == 2 then                              -- FCVTMS
                    intVal = math.floor(fpVal)
                else                                                -- FCVTZS
                    intVal = if fpVal >= 0 then math.floor(fpVal) else math.ceil(fpVal)
                end
                if sf == 1 then cpu:writeX(rd, Int.from(intVal))
                else cpu:writeW(rd, Int.band(Int.from(intVal), Int.MASK32)) end
            elseif opcode == 1 then -- FCVT*U (fp->unsigned int)
                local intVal: number
                if rmode == 3 then -- FCVTZU
                    intVal = if fpVal >= 0 then math.floor(fpVal) else 0
                else
                    intVal = math.floor(fpVal)
                end
                if sf == 1 then cpu:writeX(rd, Int.from(if intVal >= 0 then intVal else 0))
                else cpu:writeW(rd, Int.from(if intVal >= 0 then intVal else 0)) end
            elseif opcode == 2 then -- SCVTF (signed int->fp)
                local intVal = if sf == 1 then Int.toNumber(cpu:readX(rn)) else Int.toNumber(Int.signExtend(cpu:readW(rn), 32))
                writeFPD(cpu, rd, intVal)
            elseif opcode == 3 then -- UCVTF (unsigned int->fp)
                local intVal: number
                if sf == 1 then
                    -- Unsigned 64-bit to double: handle large values
                    local v = cpu:readX(rn)
                    if Int.isNegative(v) then
                        -- Treat as unsigned: value = 2^64 + signed_value
                        intVal = Int.toNumber(v) + 18446744073709551616
                    else
                        intVal = Int.toNumber(v)
                    end
                else
                    intVal = Int.toNumber(cpu:readW(rn))
                end
                writeFPD(cpu, rd, intVal)
            else
                Decode.unimplemented(cpu, insn, pc)
            end
        elseif ftype == 0 then -- source is single
            local fpVal = readFPS(cpu, rn)
            if opcode == 2 then -- SCVTF
                local intVal = if sf == 1 then Int.toNumber(cpu:readX(rn)) else Int.toNumber(Int.signExtend(cpu:readW(rn), 32))
                writeFPS(cpu, rd, intVal)
            elseif opcode == 3 then -- UCVTF
                local intVal = if sf == 1 then Int.toNumber(cpu:readX(rn)) else Int.toNumber(cpu:readW(rn))
                writeFPS(cpu, rd, intVal)
            elseif opcode == 0 then -- FCVTZS
                local intVal = if fpVal >= 0 then math.floor(fpVal) else math.ceil(fpVal)
                if sf == 1 then cpu:writeX(rd, Int.from(intVal))
                else cpu:writeW(rd, Int.band(Int.from(intVal), Int.MASK32)) end
            elseif opcode == 1 then -- FCVTZU
                local intVal = if fpVal >= 0 then math.floor(fpVal) else 0
                if sf == 1 then cpu:writeX(rd, Int.from(intVal))
                else cpu:writeW(rd, Int.from(if intVal >= 0 then intVal else 0)) end
            else
                Decode.unimplemented(cpu, insn, pc)
            end
        else
            Decode.unimplemented(cpu, insn, pc)
        end
        return
    end

    -- FMOV between FP registers: already handled by the 1-source path above (opcode=0)
    -- If we get here, it's something we missed
    Decode.unimplemented(cpu, insn, pc)
end

function Decode.execSimdLdSt(cpu: CPU.CPU, insn: number, pc: integer)
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local isLoad = bit32.band(bit32.rshift(insn, 22), 1) == 1
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local opcode = bit32.band(bit32.rshift(insn, 12), 0xF)
    local size = bit32.band(bit32.rshift(insn, 10), 0x3)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local dataBytes = if q == 1 then 16 else 8 -- bytes per register

    -- Determine number of registers based on opcode
    local numRegs: number
    if opcode == 0x0 then numRegs = 4     -- LD4/ST4
    elseif opcode == 0x2 then numRegs = 4 -- LD1x4/ST1x4
    elseif opcode == 0x4 then numRegs = 3 -- LD3/ST3
    elseif opcode == 0x6 then numRegs = 3 -- LD1x3/ST1x3
    elseif opcode == 0x7 then numRegs = 1 -- LD1/ST1 (single reg)
    elseif opcode == 0x8 then numRegs = 2 -- LD2/ST2
    elseif opcode == 0xA then numRegs = 2 -- LD1x2/ST1x2
    else numRegs = 1
    end

    local addr = cpu:readXOrSP(rn)
    local totalBytes = numRegs * dataBytes

    for reg = 0, numRegs - 1 do
        local vReg = (rt + reg) % 32
        local regAddr = Int.add(addr, Int.from(reg * dataBytes))
        if isLoad then
            local lo = cpu.mem:readU64(regAddr)
            local hi = Int.ZERO
            if q == 1 then
                hi = cpu.mem:readU64(Int.add(regAddr, Int.from(8)))
            end
            cpu:writeVFull(vReg, lo, hi)
        else
            cpu.mem:writeU64(regAddr, cpu:readVLo(vReg))
            if q == 1 then
                cpu.mem:writeU64(Int.add(regAddr, Int.from(8)), cpu:readVHi(vReg))
            end
        end
    end

    -- Writeback: only for post-index forms (bit[23]=1)
    local postIndex = bit32.band(bit32.rshift(insn, 23), 1)
    if postIndex == 1 then
        if rm == 0x1F then
            -- Immediate offset = total bytes transferred
            cpu:writeXOrSP(rn, Int.add(addr, Int.from(totalBytes)))
        else
            -- Register offset
            cpu:writeXOrSP(rn, Int.add(addr, cpu:readX(rm)))
        end
    end
end

function Decode.execSimdLdStSingle(cpu: CPU.CPU, insn: number, pc: integer)
    -- Advanced SIMD load/store single structure
    -- For now, handle the most common: LD1R (load single and replicate)
    -- and simple single-element loads/stores
    local q = bit32.band(bit32.rshift(insn, 30), 1)
    local isLoad = bit32.band(bit32.rshift(insn, 22), 1) == 1
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)
    local opcode = bit32.band(bit32.rshift(insn, 13), 0x7)
    local size = bit32.band(bit32.rshift(insn, 10), 0x3)
    local s = bit32.band(bit32.rshift(insn, 12), 1)

    local addr = cpu:readXOrSP(rn)

    -- LD1R: opcode=110, size determines element size
    if opcode == 6 then
        local elemSize = bit32.lshift(1, size) -- 1,2,4,8 bytes
        local val = Int.ZERO
        if elemSize == 1 then val = Int.from(cpu.mem:readU8(addr))
        elseif elemSize == 2 then val = Int.from(cpu.mem:readU16(addr))
        elseif elemSize == 4 then val = Int.from(cpu.mem:readU32(addr))
        else val = cpu.mem:readU64(addr) end

        -- Replicate across vector
        local lo = Int.ZERO
        local hi = Int.ZERO
        local elemBits = elemSize * 8
        local pos = 0
        while pos < 64 do
            lo = Int.bor(lo, Int.shl(val, pos))
            pos += elemBits
        end
        hi = if q == 1 then lo else Int.ZERO
        cpu:writeVFull(rt, lo, hi)
    else
        -- Simple single-element access - determine index from S:size:opcode
        -- This is complex; for now just do a basic implementation
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.doSimdLoadStore(cpu: CPU.CPU, rt: number, addr: integer, dataSize: number, isLoad: boolean)
    if isLoad then
        if dataSize == 1 then
            cpu:writeVFull(rt, Int.from(cpu.mem:readU8(addr)), Int.ZERO)
        elseif dataSize == 2 then
            cpu:writeVFull(rt, Int.from(cpu.mem:readU16(addr)), Int.ZERO)
        elseif dataSize == 4 then
            cpu:writeVFull(rt, Int.from(cpu.mem:readU32(addr)), Int.ZERO)
        elseif dataSize == 8 then
            cpu:writeVFull(rt, cpu.mem:readU64(addr), Int.ZERO)
        else -- 16
            local lo = cpu.mem:readU64(addr)
            local hi = cpu.mem:readU64(Int.add(addr, Int.from(8)))
            cpu:writeVFull(rt, lo, hi)
        end
    else
        if dataSize == 1 then
            cpu.mem:writeU8(addr, Int.toNumber(Int.band(cpu:readVLo(rt), Int.MASK8)))
        elseif dataSize == 2 then
            cpu.mem:writeU16(addr, Int.toNumber(Int.band(cpu:readVLo(rt), Int.MASK16)))
        elseif dataSize == 4 then
            cpu.mem:writeU32(addr, Int.toNumber(Int.band(cpu:readVLo(rt), Int.MASK32)))
        elseif dataSize == 8 then
            cpu.mem:writeU64(addr, cpu:readVLo(rt))
        else -- 16
            cpu.mem:writeU64(addr, cpu:readVLo(rt))
            cpu.mem:writeU64(Int.add(addr, Int.from(8)), cpu:readVHi(rt))
        end
    end
end

function Decode.execLoadLiteral(cpu: CPU.CPU, insn: number, pc: integer)
    local opc = bit32.band(bit32.rshift(insn, 30), 0x3)
    local imm19 = bit32.band(bit32.rshift(insn, 5), 0x7FFFF)
    local rt = bit32.band(insn, 0x1F)

    local offset = signExtendImm(imm19, 19)
    local addr = Int.add(pc, Int.shl(offset, 2))

    if opc == 0 then -- LDR W
        cpu:writeX(rt, Int.from(cpu.mem:readU32(addr)))
    elseif opc == 1 then -- LDR X
        cpu:writeX(rt, cpu.mem:readU64(addr))
    elseif opc == 2 then -- LDRSW
        cpu:writeX(rt, Int.signExtend(Int.from(cpu.mem:readU32(addr)), 32))
    else
        -- PRFM literal - NOP
    end
end

function Decode.execLoadStoreExclusive(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3)
    local o2 = bit32.band(bit32.rshift(insn, 23), 1)
    local isLoad = bit32.band(bit32.rshift(insn, 22), 1) == 1
    local o1 = bit32.band(bit32.rshift(insn, 21), 1)
    local rs = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local o0 = bit32.band(bit32.rshift(insn, 15), 1)
    local rt2 = bit32.band(bit32.rshift(insn, 10), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local addr = cpu:readXOrSP(rn)

    -- For single-threaded emulation, exclusive loads always succeed and stores always succeed.
    if isLoad then
        -- LDXR/LDAXR/LDAR
        if size == 0 then
            cpu:writeX(rt, Int.from(cpu.mem:readU8(addr)))
        elseif size == 1 then
            cpu:writeX(rt, Int.from(cpu.mem:readU16(addr)))
        elseif size == 2 then
            cpu:writeX(rt, Int.from(cpu.mem:readU32(addr)))
        else
            cpu:writeX(rt, cpu.mem:readU64(addr))
        end
    else
        -- STXR/STLXR/STLR
        if size == 0 then
            cpu.mem:writeU8(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK8)))
        elseif size == 1 then
            cpu.mem:writeU16(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK16)))
        elseif size == 2 then
            cpu.mem:writeU32(addr, Int.toNumber(Int.band(cpu:readX(rt), Int.MASK32)))
        else
            cpu.mem:writeU64(addr, cpu:readX(rt))
        end
        -- For exclusive stores, report success (0) in Rs
        if o1 == 1 or o2 == 0 then -- exclusive variant
            cpu:writeW(rs, Int.ZERO)
        end
    end
end

function Decode.execAtomic(cpu: CPU.CPU, insn: number, pc: integer)
    local size = bit32.band(bit32.rshift(insn, 30), 0x3)
    local opc = bit32.band(bit32.rshift(insn, 12), 0x7) -- bits [14:12]
    local rs = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rt = bit32.band(insn, 0x1F)

    local addr = cpu:readXOrSP(rn)
    local sval = cpu:readX(rs)

    -- Read old value
    local oldVal: integer
    if size == 0 then
        oldVal = Int.from(cpu.mem:readU8(addr))
    elseif size == 1 then
        oldVal = Int.from(cpu.mem:readU16(addr))
    elseif size == 2 then
        oldVal = Int.from(cpu.mem:readU32(addr))
    else
        oldVal = cpu.mem:readU64(addr)
    end

    -- Compute new value based on operation
    local newVal: integer
    if opc == 0 then -- LDADD
        newVal = Int.add(oldVal, sval)
    elseif opc == 1 then -- LDCLR
        newVal = Int.band(oldVal, Int.bnot(sval))
    elseif opc == 2 then -- LDEOR
        newVal = Int.bxor(oldVal, sval)
    elseif opc == 3 then -- LDSET
        newVal = Int.bor(oldVal, sval)
    elseif opc == 4 then -- LDSMAX
        if Int.gt(sval, oldVal) then newVal = sval else newVal = oldVal end
    elseif opc == 5 then -- LDSMIN
        if Int.lt(sval, oldVal) then newVal = sval else newVal = oldVal end
    elseif opc == 6 then -- LDUMAX
        if Int.ugt(sval, oldVal) then newVal = sval else newVal = oldVal end
    elseif opc == 7 then -- LDUMIN
        if Int.ult(sval, oldVal) then newVal = sval else newVal = oldVal end
    else
        newVal = oldVal
    end

    -- Write new value
    if size == 0 then
        cpu.mem:writeU8(addr, Int.toNumber(Int.band(newVal, Int.MASK8)))
    elseif size == 1 then
        cpu.mem:writeU16(addr, Int.toNumber(Int.band(newVal, Int.MASK16)))
    elseif size == 2 then
        cpu.mem:writeU32(addr, Int.toNumber(Int.band(newVal, Int.MASK32)))
    else
        cpu.mem:writeU64(addr, newVal)
    end

    -- Return old value in Rt
    cpu:writeX(rt, oldVal)
end

-- Also handle CAS (compare and swap) which has a different encoding
-- CAS: 11 size 0 01000 o1 1 Rs o0 11111 Rn Rt
-- Actually CAS is: size 00 01000 A 1 Rs R 11111 Rn Rt
-- Let me also check for SWP here

------------------------------------------------------------
-- Data Processing - Register
------------------------------------------------------------
function Decode.execDPReg(cpu: CPU.CPU, insn: number, pc: integer)
    -- Check if this is a SIMD/FP instruction FIRST (before the integer op0 split)
    local top8 = bit32.band(bit32.rshift(insn, 24), 0xFF)
    if bit32.band(top8, 0x9F) == 0x0E or  -- 0x0E, 0x2E, 0x4E, 0x6E
       bit32.band(top8, 0x9F) == 0x0F or  -- 0x0F, 0x2F, 0x4F, 0x6F
       bit32.band(top8, 0xBF) == 0x1E or  -- 0x1E, 0x5E
       bit32.band(top8, 0xBF) == 0x1F or  -- 0x1F, 0x5F
       top8 == 0x9E or                     -- FP<->GP (FMOV etc.)
       top8 == 0x7E then                   -- scalar SIMD
        Decode.execSimdDP(cpu, insn, pc)
        return
    end

    local op0 = bit32.band(bit32.rshift(insn, 28), 1) -- bit 28

    if op0 == 0 then
        -- Logical (shifted register), add/sub shifted/extended
        local bit24 = bit32.band(bit32.rshift(insn, 24), 1)
        if bit24 == 0 then
            -- Logical shifted register
            Decode.execLogicalShiftReg(cpu, insn, pc)
        else
            -- Add/sub shifted or extended register
            local bit21 = bit32.band(bit32.rshift(insn, 21), 1)
            if bit21 == 0 then
                Decode.execAddSubShiftReg(cpu, insn, pc)
            else
                Decode.execAddSubExtReg(cpu, insn, pc)
            end
        end
    else
        -- op0 == 1: Data processing (3-source), conditional select, data processing (2-source)
        local bits2124 = bit32.band(bit32.rshift(insn, 21), 0xF)
        if bit32.band(bits2124, 0x8) == 0x8 then
            -- Data processing 3-source (MADD, MSUB, etc.)
            Decode.execDP3Source(cpu, insn, pc)
        elseif bit32.band(bits2124, 0xE) == 0x4 then
            -- Conditional select
            Decode.execCondSelect(cpu, insn, pc)
        elseif bit32.band(bits2124, 0xE) == 0x6 then
            -- Data processing 2-source (UDIV, SDIV, etc.) or 1-source (CLZ, etc.)
            local bit30 = bit32.band(bit32.rshift(insn, 30), 1)
            if bit30 == 0 then
                Decode.execDP2Source(cpu, insn, pc)
            else
                Decode.execDP1Source(cpu, insn, pc)
            end
        elseif bit32.band(bits2124, 0xE) == 0x0 then
            -- Add/sub with carry
            Decode.execAddSubCarry(cpu, insn, pc)
        elseif bit32.band(bits2124, 0xE) == 0x2 then
            -- Conditional compare
            Decode.execCondCompare(cpu, insn, pc)
        else
            Decode.unimplemented(cpu, insn, pc)
        end
    end
end

function Decode.execLogicalShiftReg(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local opc = bit32.band(bit32.rshift(insn, 29), 0x3)
    local shiftType = bit32.band(bit32.rshift(insn, 22), 0x3)
    local isNot = bit32.band(bit32.rshift(insn, 21), 1) == 1
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local imm6 = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local is32 = (sf == 0)
    local operand1 = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local operand2 = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)
    operand2 = applyShift(operand2, shiftType, imm6, is32)
    if isNot then operand2 = if is32 then Int.band(Int.bnot(operand2), Int.MASK32) else Int.bnot(operand2) end

    local result: integer
    if opc == 0 then -- AND / BIC
        result = Int.band(operand1, operand2)
    elseif opc == 1 then -- ORR / ORN
        result = Int.bor(operand1, operand2)
    elseif opc == 2 then -- EOR / EON
        result = Int.bxor(operand1, operand2)
    else -- ANDS / BICS (3)
        result = Int.band(operand1, operand2)
        if sf == 1 then cpu:setNZ64(result) else cpu:setNZ32(result) end
        cpu.C = false
        cpu.V = false
    end

    if is32 then result = Int.band(result, Int.MASK32) end

    if opc == 3 then
        if rd ~= 31 then
            if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
        end
    else
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    end
end

function Decode.execAddSubShiftReg(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1)
    local setFlags = bit32.band(bit32.rshift(insn, 29), 1) == 1
    local shiftType = bit32.band(bit32.rshift(insn, 22), 0x3)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local imm6 = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local is32 = (sf == 0)
    local operand1 = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local operand2 = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)
    operand2 = applyShift(operand2, shiftType, imm6, is32)

    local result: integer
    if op == 0 then -- ADD
        if setFlags then
            result = if sf == 1 then cpu:addWithCarry64(operand1, operand2, false)
                     else cpu:addWithCarry32(operand1, operand2, false)
        else
            result = Int.add(operand1, operand2)
            if is32 then result = Int.band(result, Int.MASK32) end
        end
    else -- SUB
        local negOp2 = if is32 then Int.band(Int.bnot(operand2), Int.MASK32) else Int.bnot(operand2)
        if setFlags then
            result = if sf == 1 then cpu:addWithCarry64(operand1, negOp2, true)
                     else cpu:addWithCarry32(operand1, negOp2, true)
        else
            result = Int.sub(operand1, operand2)
            if is32 then result = Int.band(result, Int.MASK32) end
        end
    end

    if setFlags then
        if rd ~= 31 then
            if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
        end
    else
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    end
end

function Decode.execAddSubExtReg(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1)
    local setFlags = bit32.band(bit32.rshift(insn, 29), 1) == 1
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local option = bit32.band(bit32.rshift(insn, 13), 0x7)
    local imm3 = bit32.band(bit32.rshift(insn, 10), 0x7)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local is32 = (sf == 0)
    local operand1 = if sf == 1 then cpu:readXOrSP(rn) else cpu:readWOrSP(rn)
    local operand2 = applyExtend(cpu:readX(rm), option, imm3)
    if is32 then operand2 = Int.band(operand2, Int.MASK32) end

    local result: integer
    if op == 0 then -- ADD
        if setFlags then
            result = if sf == 1 then cpu:addWithCarry64(operand1, operand2, false)
                     else cpu:addWithCarry32(operand1, operand2, false)
        else
            result = Int.add(operand1, operand2)
            if is32 then result = Int.band(result, Int.MASK32) end
        end
    else -- SUB
        local negOp2 = if is32 then Int.band(Int.bnot(operand2), Int.MASK32) else Int.bnot(operand2)
        if setFlags then
            result = if sf == 1 then cpu:addWithCarry64(operand1, negOp2, true)
                     else cpu:addWithCarry32(operand1, negOp2, true)
        else
            result = Int.sub(operand1, operand2)
            if is32 then result = Int.band(result, Int.MASK32) end
        end
    end

    if setFlags then
        if rd ~= 31 then
            if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
        end
    else
        if sf == 1 then cpu:writeXOrSP(rd, result) else cpu:writeWOrSP(rd, result) end
    end
end

function Decode.execDP3Source(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op31 = bit32.band(bit32.rshift(insn, 21), 0x7)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local o0 = bit32.band(bit32.rshift(insn, 15), 1)
    local ra = bit32.band(bit32.rshift(insn, 10), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    if sf == 1 and op31 == 0 then
        -- MADD/MSUB (64-bit)
        local a = cpu:readX(rn)
        local b = cpu:readX(rm)
        local c = cpu:readX(ra)
        local product = Int.mul(a, b)
        local result = if o0 == 0 then Int.add(c, product) else Int.sub(c, product)
        cpu:writeX(rd, result)
    elseif sf == 0 and op31 == 0 then
        -- MADD/MSUB (32-bit)
        local a = cpu:readW(rn)
        local b = cpu:readW(rm)
        local c = cpu:readW(ra)
        local product = Int.band(Int.mul(a, b), Int.MASK32)
        local result = if o0 == 0 then Int.band(Int.add(c, product), Int.MASK32)
                       else Int.band(Int.sub(c, product), Int.MASK32)
        cpu:writeW(rd, result)
    elseif sf == 1 and op31 == 1 then
        -- SMADDL/SMSUBL: signed 32x32 -> 64
        local a = Int.signExtend(cpu:readW(rn), 32)
        local b = Int.signExtend(cpu:readW(rm), 32)
        local c = cpu:readX(ra)
        local product = Int.mul(a, b)
        local result = if o0 == 0 then Int.add(c, product) else Int.sub(c, product)
        cpu:writeX(rd, result)
    elseif sf == 1 and op31 == 5 then
        -- UMADDL/UMSUBL: unsigned 32x32 -> 64
        local a = Int.band(cpu:readX(rn), Int.MASK32)
        local b = Int.band(cpu:readX(rm), Int.MASK32)
        local c = cpu:readX(ra)
        local product = Int.mul(a, b)
        local result = if o0 == 0 then Int.add(c, product) else Int.sub(c, product)
        cpu:writeX(rd, result)
    elseif sf == 1 and op31 == 2 then
        -- SMULH
        local a = cpu:readX(rn)
        local b = cpu:readX(rm)
        cpu:writeX(rd, Int.smulh(a, b))
    elseif sf == 1 and op31 == 6 then
        -- UMULH
        local a = cpu:readX(rn)
        local b = cpu:readX(rm)
        cpu:writeX(rd, Int.umulh(a, b))
    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execCondSelect(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local cond = bit32.band(bit32.rshift(insn, 12), 0xF)
    local op2 = bit32.band(bit32.rshift(insn, 10), 0x3)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local valN = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local valM = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)

    local result: integer
    if cpu:evalCondition(cond) then
        result = valN
    else
        -- Apply transformation to valM based on op and op2
        if op == 0 and op2 == 0 then -- CSEL
            result = valM
        elseif op == 0 and op2 == 1 then -- CSINC
            result = Int.add(valM, Int.ONE)
            if sf == 0 then result = Int.band(result, Int.MASK32) end
        elseif op == 1 and op2 == 0 then -- CSINV
            result = Int.bnot(valM)
            if sf == 0 then result = Int.band(result, Int.MASK32) end
        elseif op == 1 and op2 == 1 then -- CSNEG
            result = Int.neg(valM)
            if sf == 0 then result = Int.band(result, Int.MASK32) end
        else
            result = valM
        end
    end

    if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
end

function Decode.execDP2Source(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local opcode = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local is32 = (sf == 0)

    if opcode == 2 then -- UDIV
        local a = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local b = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)
        local result: integer
        if Int.isZero(b) then
            result = Int.ZERO
        else
            result = Int.udiv(a, b)
        end
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 3 then -- SDIV
        local a = if sf == 1 then cpu:readX(rn) else Int.signExtend(cpu:readW(rn), 32)
        local b = if sf == 1 then cpu:readX(rm) else Int.signExtend(cpu:readW(rm), 32)
        local result: integer
        if Int.isZero(b) then
            result = Int.ZERO
        else
            result = Int.sdiv(a, b)
        end
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 8 then -- LSLV
        local a = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local shift = Int.toNumber(Int.band(cpu:readX(rm), Int.from(if is32 then 31 else 63)))
        local result = Int.shl(a, shift)
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 9 then -- LSRV
        local a = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local shift = Int.toNumber(Int.band(cpu:readX(rm), Int.from(if is32 then 31 else 63)))
        local result = Int.shr(a, shift)
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 10 then -- ASRV
        local a = if sf == 1 then cpu:readX(rn) else Int.signExtend(cpu:readW(rn), 32)
        local shift = Int.toNumber(Int.band(cpu:readX(rm), Int.from(if is32 then 31 else 63)))
        local result = Int.sar(a, shift)
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 11 then -- RORV
        local a = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local width = if is32 then 32 else 64
        local shift = Int.toNumber(Int.band(cpu:readX(rm), Int.from(width - 1)))
        local result = if shift == 0 then a else Int.bor(Int.shr(a, shift), Int.shl(a, width - shift))
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execDP1Source(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local opcode = bit32.band(bit32.rshift(insn, 10), 0x3F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local is32 = (sf == 0)

    if opcode == 0 then -- RBIT
        local val = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local result: integer
        if is32 then
            result = Int.band(Int.shr(Int.rbit64(Int.band(val, Int.MASK32)), 32), Int.MASK32)
        else
            result = Int.rbit64(val)
        end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 1 then -- REV16
        local val = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local result = Int.rev16(val)
        if is32 then result = Int.band(result, Int.MASK32) end
        if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
    elseif opcode == 2 then -- REV32 (for 64-bit) or REV (for 32-bit)
        local val = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        if is32 then
            cpu:writeW(rd, Int.rev32(val))
        else
            -- REV32: reverse bytes in each 32-bit word
            local lo = Int.rev32(Int.band(val, Int.MASK32))
            local hi = Int.rev32(Int.band(Int.shr(val, 32), Int.MASK32))
            cpu:writeX(rd, Int.bor(lo, Int.shl(hi, 32)))
        end
    elseif opcode == 3 then -- REV (64-bit)
        local val = cpu:readX(rn)
        cpu:writeX(rd, Int.rev64(val))
    elseif opcode == 4 then -- CLZ
        local val = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local result: number
        if is32 then
            -- CLZ of 32-bit value: count leading zeros in 64-bit and subtract 32
            result = Int.clz64(Int.band(val, Int.MASK32)) - 32
        else
            result = Int.clz64(val)
        end
        if sf == 1 then cpu:writeX(rd, Int.from(result)) else cpu:writeW(rd, Int.from(result)) end
    elseif opcode == 5 then -- CLS (count leading sign bits)
        local val = if sf == 1 then cpu:readX(rn) else Int.signExtend(cpu:readW(rn), 32)
        local width = if is32 then 32 else 64
        -- CLS = CLZ(val XOR (val << 1)) for the appropriate width
        local shifted = Int.shl(val, 1)
        local xored = Int.bxor(val, shifted)
        if is32 then xored = Int.band(xored, Int.MASK32) end
        local result = (if is32 then Int.clz64(xored) - 32 else Int.clz64(xored))
        if sf == 1 then cpu:writeX(rd, Int.from(result)) else cpu:writeW(rd, Int.from(result)) end
    else
        Decode.unimplemented(cpu, insn, pc)
    end
end

function Decode.execAddSubCarry(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1)
    local setFlags = bit32.band(bit32.rshift(insn, 29), 1) == 1
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local rd = bit32.band(insn, 0x1F)

    local operand1 = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
    local operand2 = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)

    if op == 1 then -- SBC
        operand2 = if sf == 0 then Int.band(Int.bnot(operand2), Int.MASK32) else Int.bnot(operand2)
    end

    local result: integer
    if setFlags then
        result = if sf == 1 then cpu:addWithCarry64(operand1, operand2, if op == 1 then cpu.C else cpu.C)
                 else cpu:addWithCarry32(operand1, operand2, if op == 1 then cpu.C else cpu.C)
    else
        local carry: integer = if cpu.C then Int.ONE else Int.ZERO
        result = Int.add(Int.add(operand1, operand2), carry)
        if sf == 0 then result = Int.band(result, Int.MASK32) end
    end

    if sf == 1 then cpu:writeX(rd, result) else cpu:writeW(rd, result) end
end

function Decode.execCondCompare(cpu: CPU.CPU, insn: number, pc: integer)
    local sf = bit32.band(bit32.rshift(insn, 31), 1)
    local op = bit32.band(bit32.rshift(insn, 30), 1) -- 0=CCMN, 1=CCMP
    local isImm = bit32.band(bit32.rshift(insn, 11), 1) == 1
    local rm = bit32.band(bit32.rshift(insn, 16), 0x1F)
    local cond = bit32.band(bit32.rshift(insn, 12), 0xF)
    local rn = bit32.band(bit32.rshift(insn, 5), 0x1F)
    local nzcv = bit32.band(insn, 0xF)

    if cpu:evalCondition(cond) then
        local operand1 = if sf == 1 then cpu:readX(rn) else cpu:readW(rn)
        local operand2: integer
        if isImm then
            operand2 = Int.from(rm) -- imm5 is in the rm field
        else
            operand2 = if sf == 1 then cpu:readX(rm) else cpu:readW(rm)
        end

        if op == 1 then -- CCMP (subtract)
            operand2 = if sf == 0 then Int.band(Int.bnot(operand2), Int.MASK32) else Int.bnot(operand2)
            if sf == 1 then cpu:addWithCarry64(operand1, operand2, true)
            else cpu:addWithCarry32(operand1, operand2, true) end
        else -- CCMN (add)
            if sf == 1 then cpu:addWithCarry64(operand1, operand2, false)
            else cpu:addWithCarry32(operand1, operand2, false) end
        end
    else
        -- Condition not met: set NZCV to the immediate value
        cpu.N = bit32.band(nzcv, 8) ~= 0
        cpu.Z = bit32.band(nzcv, 4) ~= 0
        cpu.C = bit32.band(nzcv, 2) ~= 0
        cpu.V = bit32.band(nzcv, 1) ~= 0
    end
end

return Decode
